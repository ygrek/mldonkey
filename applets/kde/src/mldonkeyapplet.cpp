/*****************************************************************

Copyright (c) 2001 Matthias Elter <elter@kde.org>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

******************************************************************/

#include <stdlib.h>
#include <time.h>

#include <qlabel.h>
#include <qlayout.h>
#include <qpainter.h>
#include <qpopupmenu.h>

#include <klocale.h>
#include <kglobal.h>
#include <kdebug.h>
#include <kconfig.h>
#include <kmessagebox.h>
#include <kaboutdata.h>
#include <kaboutapplication.h>

#include "mldonkeyapplet.h"
#include "mldonkeyapplet.moc"

extern "C"
{
    KPanelApplet* init(QWidget *parent, const QString& configFile)
    {
        KGlobal::locale()->insertCatalogue("mldonkeyapplet");
        return new MLDonkeyApplet(configFile, KPanelApplet::Normal,
                                 KPanelApplet::About, parent, "mldonkeyapplet");
    }
}

MLDonkeyApplet::MLDonkeyApplet(const QString& configFile, Type type, int actions,
                               QWidget *parent, const char *name)
    : KPanelApplet(configFile, type, actions, parent, name), _aboutData(0)
{
    // setup table
    _table = new PiecesTable(this);

    // setup layout
    QHBoxLayout *_layout = new QHBoxLayout(this);
    _layout->add(_table);

    srand(time(0));
}

int MLDonkeyApplet::widthForHeight(int h) const
{
    return h; // we want to be quadratic
}

int MLDonkeyApplet::heightForWidth(int w) const
{
    return w; // we want to be quadratic
}

void MLDonkeyApplet::about()
{
    if(!_aboutData) {
	_aboutData = new KAboutData("mldonkeyapplet", I18N_NOOP("KMLDonkeyApplet"), "1.0",
                                    I18N_NOOP("MLDonkey pieces applet.\n\n"
                                              "The goal is to put the sliding pieces into numerical order.\n"
                                              "Select \"Randomize Pieces\" from the RMB menu to start a game."),
                                    KAboutData::License_BSD, "(c) 2001, Matthias Elter");
	_aboutData->addAuthor("Matthias Elter", 0, "elter@kde.org");
    }

    KAboutApplication dialog(_aboutData);
    dialog.show();
}

PiecesTable::PiecesTable(QWidget* parent, const char* name )
    : QTableView(parent, name), _menu(0), _activeRow(-1), _activeCol(-1), _randomized(false)
{
    // setup table view
    setFrameStyle(StyledPanel | Sunken);
    setBackgroundMode(NoBackground);
    setMouseTracking(true);

    setNumRows(4);
    setNumCols(4);

    // init arrays
    initMap();
    initColors();
}

void PiecesTable::paintCell(QPainter *p, int row, int col)
{
    int w = cellWidth();
    int h = cellHeight();
    int x2 = w - 1;
    int y2 = h - 1;

    int number = _map[col + row * numCols()] + 1;

    bool active = (row == _activeRow && col == _activeCol);

    // draw cell background
    if(number == 16)
        p->setBrush(colorGroup().background());
    else
        p->setBrush(_colors[number-1]);
    p->setPen(NoPen);
    p->drawRect(0, 0, w, h);

    // draw borders
    if (height() > 40) {
        p->setPen(colorGroup().text());
        if(col < numCols()-1)
            p->drawLine(x2, 0, x2, y2); // right border line

        if(row < numRows()-1)
            p->drawLine(0, y2, x2, y2); // bottom boder line
    }

    // draw number
    if (number == 16) return;
    if(active)
        p->setPen(white);
    else
        p->setPen(black);
    p->drawText(0, 0, x2, y2, AlignHCenter | AlignVCenter, QString::number(number));
}

void PiecesTable::resizeEvent(QResizeEvent *e)
{
    QTableView::resizeEvent(e);

    // set font
    QFont f = font();
    if (height() > 50)
        f.setPixelSize(8);
    else if (height() > 40)
        f.setPixelSize(7);
    else if (height() > 24)
        f.setPixelSize(5);
    else
        f.setPixelSize(3);

    setFont(f);

    setCellWidth(contentsRect().width()/ numRows());
    setCellHeight(contentsRect().height() / numCols());
}

void PiecesTable::initColors()
{
    _colors.resize(numRows() * numCols());
    for (int r = 0; r < numRows(); r++)
        for (int c = 0; c < numCols(); c++)
            _colors[c + r *numCols()] = QColor(255 - 70 * c,255 - 70 * r, 150);
}

void PiecesTable::initMap()
{
    _map.resize(16);
    for (unsigned int i = 0; i < 16; i++)
        _map[i] = i;

    _randomized = false;
}

void PiecesTable::randomizeMap()
{
    QArray<int> positions;
    positions.fill(0, 16);

    for (unsigned int i = 0; i < 16; i++) {
        while(1) {
            int r = (int) (((double)rand() / RAND_MAX) * 16);
            if(positions[r] == 0) {
                _map[i] = r;
                positions[r] = 1;
                break;
            }
        }
    }
    repaint();
    _randomized = true;
}

void PiecesTable::checkwin()
{
    if(!_randomized) return;

    int i;
    for (i = 0; i < 16; i++)
        if(i != _map[i])
            break;

    if (i == 16)
        KMessageBox::information(this, i18n("Congratulations!\nYou win the game!"), i18n("MLDonkey Pieces"));
}

void PiecesTable::mousePressEvent(QMouseEvent* e)
{
    QTableView::mousePressEvent(e);

    if (e->button() == RightButton) {

        // setup RMB pupup menu
        if(!_menu) {
            _menu = new QPopupMenu(this);
            _menu->insertItem(i18n("R&andomize Pieces"), mRandomize);
            _menu->insertItem(i18n("&Reset Pieces"), mReset);
            _menu->adjustSize();
        }

        // execute RMB popup and check result
        switch(_menu->exec(mapToGlobal(e->pos()))) {
            case mRandomize:
                randomizeMap();
                break;
            case mReset:
                initMap();
                repaint();
                break;
            default:
                break;
        }
    }
    else {
        // GAME LOGIC

        // find the free position
        int pos = _map.find(15);
        if(pos < 0) return;

        int frow = pos / numCols();
        int fcol = pos - frow * numCols();

        // find click position
        int row = findRow(e->y());
        int col = findCol(e->x());

        // sanity check
        if (row < 0 || row >= numRows()) return;
        if (col < 0 || col >= numCols()) return;

        // valid move?
        if(row != frow && col != fcol) return;

        // rows match -> shift pieces
        if(row == frow) {

            if (col < fcol) {
                for(int c = fcol; c > col; c--) {
                    _map[c + row * numCols()] = _map[ c-1 + row *numCols()];
                    updateCell(row, c, false);
                }
            }
            else if (col > fcol) {
                for(int c = fcol; c < col; c++) {
                    _map[c + row * numCols()] = _map[ c+1 + row *numCols()];
                    updateCell(row, c, false);
                }
            }
        }
        // cols match -> shift pieces
        else if (col == fcol) {

            if (row < frow) {
                for(int r = frow; r > row; r--) {
                    _map[col + r * numCols()] = _map[ col + (r-1) *numCols()];
                    updateCell(r, col, false);
                }
            }
            else if (row > frow) {
                for(int r = frow; r < row; r++) {
                    _map[col + r * numCols()] = _map[ col + (r+1) *numCols()];
                    updateCell(r, col, false);
                }
            }
        }
        // move free cell to click position
        _map[col + row * numCols()] = 15;
        updateCell(row, col, false);

        // check if the player wins with this move
        checkwin();
    }
}

void PiecesTable::mouseMoveEvent(QMouseEvent* e)
{
    QTableView::mouseMoveEvent(e);

    // highlight on mouse over
    int row = findRow(e->y());
    int col = findCol(e->x());

    int oldrow = _activeRow;
    int oldcol = _activeCol;

    if(row >= numRows()
       || col >= numCols()
       || row < 0
       || col < 0) {
        _activeRow = -1;
        _activeCol = -1;
    }
    else {
        _activeRow = row;
        _activeCol = col;
    }

    updateCell(oldrow, oldcol, false);
    updateCell(row, col, false);
}
