package com.igormaznitsa.jprol.easygui;

import java.awt.Font;
import java.awt.geom.AffineTransform;
import java.io.InputStream;
import java.text.AttributedCharacterIterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;


public enum LocalFont {
  LOCAL_NOTO_SANS_MONO("Internal Noto Sans Mono Regular", "/fonts/NotoSansMono-Regular.ttf"),
  LOCAL_JET_BRAINS_MONO("Internal JetBrains Mono Regular", "/fonts/JetBrainsMono-Regular.ttf");

  public static final List<LocalFont> VALUES = List.of(LocalFont.values());

  private final String title;
  private final LocallyLoadedFont font;

  LocalFont(final String title, final String resource) {
    this.title = title;
    try (final InputStream inputStream = Objects.requireNonNull(
        LocalFont.class.getResourceAsStream(resource))) {
      this.font = new LocallyLoadedFont(this, Font.createFont(Font.TRUETYPE_FONT, inputStream));
    } catch (Exception ex) {
      throw new Error("Can't load font resource: " + resource, ex);
    }
  }

  public static String[] allAllowedStyles() {
    return new String[] {"PLAIN", "BOLD", "ITALIC", "BOLD+ITALIC"};
  }

  public static String styleAsString(final Font font) {
    final int fontStyle = font.getStyle();
    if (fontStyle == (Font.BOLD | Font.ITALIC)) {
      return "BOLD+ITALIC";
    }
    if (fontStyle == Font.BOLD) {
      return "BOLD";
    }
    if (fontStyle == Font.ITALIC) {
      return "ITALIC";
    }
    return "PLAIN";
  }

  public static String makeFontDescriptor(final Font font) {
    if (font instanceof LocallyLoadedFont) {
      return ((LocallyLoadedFont) font).getLocalFont().name() + ' ' + styleAsString(font) + ' ' +
          font.getSize();
    } else {
      return font.getFamily(Locale.ENGLISH) + ' ' + styleAsString(font) + ' ' + font.getSize();
    }
  }

  public static Font decodeFont(final String descriptor) {
    final String[] split = descriptor.split("\\s");
    final String family = split.length > 0 ? split[0].trim() : LocalFont.LOCAL_NOTO_SANS_MONO.title;
    final String style = split.length > 1 ? split[1].trim() : "PLAIN";
    final String size = split.length > 2 ? split[2].trim() : "18";
    int sizeInt = 18;
    try {
      sizeInt = Math.max(1, Integer.parseInt(size));
    } catch (Exception ex) {
      // do nothing
    }
    return decodeFont(family, style, sizeInt);
  }

  public static int dscodeStyle(final String style) {
    final String normalized = style.toLowerCase(Locale.ENGLISH);
    int styleAccum = 0;
    if (normalized.contains("plain")) {
      styleAccum |= Font.PLAIN;
    }
    if (normalized.contains("bold")) {
      styleAccum |= Font.BOLD;
    }
    if (normalized.contains("italic")) {
      styleAccum |= Font.ITALIC;
    }
    return styleAccum;
  }

  public static Font decodeFont(final String familyName, final String style, final int size) {
    final int resultStyle = dscodeStyle(style);
    return findForName(familyName).map(localFont -> (Font) new LocallyLoadedFont(localFont,
            localFont.getFont().deriveFont(resultStyle, size)))
        .orElseGet(() -> Font.decode(familyName + ' ' + style + ' ' + size));
  }

  public static Optional<LocalFont> findForName(final String name) {
    return VALUES.stream().filter(x -> x.name().equalsIgnoreCase(name)).findFirst();
  }

  public static Optional<LocalFont> findForTitle(final String title) {
    return VALUES.stream().filter(x -> x.getTitle().equalsIgnoreCase(title)).findFirst();
  }

  public String getTitle() {
    return this.title;
  }

  public Font getFont() {
    return this.font;
  }

  public static class LocallyLoadedFont extends Font {
    private final LocalFont localFont;

    public LocallyLoadedFont(final LocalFont localFont, String name, int style, int size) {
      super(name, style, size);
      this.localFont = localFont;
    }

    public LocallyLoadedFont(
        LocalFont localFont,
        Map<? extends AttributedCharacterIterator.Attribute, ?> attributes) {
      super(attributes);
      this.localFont = localFont;
    }

    public LocallyLoadedFont(LocalFont localFont, Font font) {
      super(font);
      this.localFont = localFont;
    }

    public LocalFont getLocalFont() {
      return this.localFont;
    }

    @Override
    public Font deriveFont(int style, float size) {
      return new LocallyLoadedFont(this.localFont, super.deriveFont(style, size));
    }

    @Override
    public Font deriveFont(int style, AffineTransform trans) {
      return new LocallyLoadedFont(localFont, super.deriveFont(style, trans));
    }

    @Override
    public Font deriveFont(float size) {
      return new LocallyLoadedFont(this.localFont, super.deriveFont(size));
    }

    @Override
    public Font deriveFont(AffineTransform trans) {
      return new LocallyLoadedFont(this.localFont, super.deriveFont(trans));
    }

    @Override
    public Font deriveFont(int style) {
      return new LocallyLoadedFont(this.localFont, super.deriveFont(style));
    }

    @Override
    public Font deriveFont(Map<? extends AttributedCharacterIterator.Attribute, ?> attributes) {
      return new LocallyLoadedFont(this.localFont, super.deriveFont(attributes));
    }
  }
}
