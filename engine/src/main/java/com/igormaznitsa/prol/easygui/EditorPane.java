/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prol.easygui;

import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * The class implements the Editor pane for the IDE.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class EditorPane extends JTextPane {

    private static final long serialVersionUID = 2384993058573194751L;
    private volatile JPopupMenu popupMenu;
    private volatile EventReplacer replacer;
    public EditorPane() {
        super();

        addMouseListener(new MouseAdapter() {

            @Override
            public void mousePressed(MouseEvent e) {
                showMenuIfPopupTrigger(e);
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                showMenuIfPopupTrigger(e);
            }
        });
    }

    public void setEventReplacer(final EventReplacer replacer) {
        this.replacer = replacer;
    }

    public void setPopupMenu(final JPopupMenu menu) {
        this.popupMenu = menu;
    }

    private void showMenuIfPopupTrigger(final MouseEvent e) {
        final JPopupMenu thePopup = this.popupMenu;

        if (thePopup == null) {
            return;
        }
        if (e.isPopupTrigger()) {
            thePopup.show(this, e.getX() + 3, e.getY() + 3);
        }
    }

    @Override
    public void paste() {
        if (replacer == null) {
            super.paste();
        } else {
            replacer.pasteEvent();
        }
    }

    public interface EventReplacer {
        void pasteEvent();
    }
}
