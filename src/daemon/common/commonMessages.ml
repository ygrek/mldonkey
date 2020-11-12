(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Printf2
open Gettext
open Options
open Str (* global_replace *)


let _s x = _s "CommonMessages" x
let _b x = _b "CommonMessages" x

let message_file_name = try
    Sys.getenv "MLDONKEY_MESSAGES"
  with _ ->
      Filename.concat CommonOptions.home_dir "messages.ini"


let message_file = Options.create_options_file message_file_name
let message_section = file_section message_file [] ""

let message name t x = define_option message_section [name] "" t x
let string name x = define_option message_section [name] "" string_option x

(* Please do not modify *_mods0, add/modify your own html_mods_theme *)

(* Style 0 *)

let html_css_mods0 = define_option message_section ["html_css_mods0"]
  "Main CSS style 0"
    string_option
"
body {
  background: @color_background@;
  margin-top: 3px;
  margin-left: 5px;
  margin-right: 5px;
  font-family: Verdana, sans-serif;
  font-size: 12px;
  }
table.commands {
  border: @color_general_border@ solid 1px;
  background: @color_background@;
  }
table.topcommands {
  background: @color_background@;
  border: @color_general_border@ solid 1px;
  border-top: @color_scrollbar_highlight@ solid 1px;
  border-left: @color_scrollbar_highlight@ solid 1px;
  }
pre {
  color: @color_general_text@;
  font-family: Courier, Arial, Helvetica, sans-serif;
  font-size: 12px;
  }
p {
  color: @color_general_text@;
  font-family: Verdana, Courier, Arial, Helvetica, sans-serif;
  font-size: 12px;
  }
input.txt {
  background: @color_input_text@;
  }
input.txt2 {
  background: @color_bbig_background@;
  font: 12px courier;
  padding: 0px;
  width: 38px;
  height: 18px;
  line-height: 14px;
  color: @color_general_text@;
  border-right: @color_some_border@ 2px solid;
  border-top: @color_general_border@ 1px solid;
  border-left: @color_general_border@ 1px solid;
  border-bottom: @color_some_border@ 2px solid;
  }
input.but2 {
  background: @color_bsmall3@;
  color: @color_general_text@;
  border: 0px;
  padding: 0px;
  font: bold 10px verdana;
  width: 36px;
  height: 14px;
  }
input.but {
  background: @color_input_button@;
  }
input.changed { border: 2px solid red; }
a:link, a:active, a:visited {
  text-decoration: none;
  font-family: verdana;
  font-size: 10px;
  color: @color_anchor@;
  }
a:hover {
  color: @color_anchor_hover@;
  text-decoration: underline;
  }
.bu {
  vertical-align: middle;
  white-space: nowrap;
  background: @color_chunk3@;
  color: @color_foreground_text_for_top_buttons@;
  font-family: Verdana;
  font-size: 9px;
  line-height: 12px;
  margin-top: 0px;
  margin-bottom: 0px;
  padding-left: 6px;
  padding-right: 6px;
  padding-top: 1px;
  padding-bottom: 1px;
  border: @color_some_border@ 0px solid;
  }
.bbig {
  text-align: center;
  font-size: 10px;
  font-family: Verdana;
  font-weight: 500;
  border-top: @color_scrollbar_highlight@ 1px solid;
  border-left: @color_scrollbar_highlight@ 1px solid;
  border-bottom: @color_general_border@ 1px solid;
  border-right: @color_general_border@ 1px solid;
  padding-left: 4px;
  padding-right: 4px;
  padding-top: 1px;
  padding-bottom: 1px;
  color: @color_general_text@;
  background: @color_bbig_background@;
  }
.bbigm {
  text-align: center;
  font: bold 10px verdana;
  border-top: @color_scrollbar_highlight@ 1px solid;
  border-left: @color_scrollbar_highlight@ 1px solid;
  border-bottom: @color_general_border@ 1px solid;
  border-right: @color_general_border@ 1px solid;
  padding-left: 4px;
  padding-right: 4px;
  padding-top: 1px;
  padding-bottom: 1px;
  color: @color_general_text@;
  background: @color_bsmall3@;
  }
.bsmall {
  background: @color_bsmall_back@;
  }
.bsmall1 {
  background: @color_bbig_background@;
  }
.bsmall2 {
  background: @color_bsmall2@;
  }
.bsmall3 {
  background: @color_bsmall3@;
  }
.bbig2 {
  background: @color_bsmall3@;
  }
.bbig3 {
  background: @color_scrollbar_face@;
  }
.header2 {
  color: @color_header2_text@;
  }
.header3 {
  color: @color_header3_text@;
  }
.b1 {
  border-left: @color_border_of_top_buttons@ solid 1px;
  border-top: @color_border_of_top_buttons@ solid 1px;
  border-right: @color_border_of_top_buttons@ solid 1px;
  border-bottom: @color_border_of_top_buttons@ solid 1px;
  }
.b2 {
  border-left: @color_border_of_top_buttons@ solid 0px;
  border-top: @color_border_of_top_buttons@ solid 1px;
  border-right: @color_border_of_top_buttons@ solid 1px;
  border-bottom: @color_border_of_top_buttons@ solid 1px;
  }
.b3 {
  border-left: @color_border_of_top_buttons@ solid 1px;
  border-top: @color_border_of_top_buttons@ solid 0px;
  border-right: @color_border_of_top_buttons@ solid 1px;
  border-bottom: @color_border_of_top_buttons@ solid 1px;
  }
.b4 {
  border-left: @color_border_of_top_buttons@ solid 0px;
  border-top: @color_border_of_top_buttons@ solid 0px;
  border-right: @color_border_of_top_buttons@ solid 1px;
  border-bottom: @color_border_of_top_buttons@ solid 1px;
  }
.bb1 {
  border-left: @color_general_border@ solid 1px;
  border-top: @color_scrollbar_highlight@ solid 1px;
  border-right: @color_scrollbar_highlight@ solid 1px;
  border-bottom: @color_general_border@ solid 1px;
  }
.bb2 {
  border-left: @color_big_buttons_and_border_highlight@ solid 1px;
  border-top: @color_scrollbar_highlight@ solid 1px;
  border-right: @color_scrollbar_highlight@ solid 0px;
  border-bottom: @color_general_border@ solid 1px;
  }
.bb3 {
  border-left: @color_big_buttons_and_border_highlight@ solid 1px;
  border-top: @color_scrollbar_highlight@ solid 1px;
  border-right: @color_general_border@ solid 0px;
  border-bottom: @color_general_border@ solid 0px;
  }
.bb4 {
  border-left: @color_big_buttons_and_border_highlight@ solid 1px;
  border-top: @color_scrollbar_highlight@ solid 1px;
  border-right: @color_general_border@ solid 1px;
  border-bottom: @color_general_border@ solid 0px;
  }
.src {
  border-left: @color_general_border@ solid 0px;
  border-top: @color_general_border@ solid 0px;
  border-right: @color_general_border@ solid 1px;
  border-bottom: @color_general_border@ solid 1px;
  }
.srctd {
  font-family: Verdana;
  font-size: 8px;
  }
.hcenter {
  margin-left: auto;
  margin-right: auto;
}
td.popupstyle {
  background: @color_background_popup@;
  }
td.txt {
  color: @color_general_text@;
  }
td.fbig {
  color: @color_general_text@;
  cursor: pointer;
  padding-left: 2px;
  padding-right: 2px;
  font-family: Verdana;
  font-size: 10px;
  background: @color_fbig_background@;
  border-top: @color_general_border@ solid 1px;
  border-left: @color_general_border@ solid 1px;
  }
td.pr {
  border-right: @color_general_border@ solid 1px;
  }
td.fbigb {
  border-top: @color_general_border@ solid 0px;
  border-bottom: @color_general_border@ solid 1px;
  }
td.fbigpad {
  padding-top: 2px;
  padding-bottom: 2px;
  }
td, tr {
  font-size: 12px;
  font-family: verdana;
  }
td.sr {
  white-space: nowrap;
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 4px;
  padding-right: 4px;
  font-family: verdana;
  font-size: 10px;
  color: @color_general_text@;
  }
td.srp {
  white-space: nowrap;
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 0px;
  padding-right: 4px;
  font-family: verdana;
  font-size: 10px;
  color: @color_one_td_text@;
  }
td.srw {
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 4px;
  padding-right: 4px;
  font-family: verdana;
  font-size: 10px;
  color: @color_general_text@;
  }
td.srh {
  cursor: pointer;
  vertical-align: top;
  background: @color_table_header_background@;
  white-space: nowrap;
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 4px;
  padding-right: 4px;
  font-family: verdana;
  font-size: 10px;
  color: @color_general_text@;
  }
td.total {
  border-top: @color_general_border@ solid 1px;
  border-bottom: @color_general_border@ solid 1px;
  }
tr.dl-1, td.dl-1 {
  background: @color_dl1_back@;
  }
tr.dl-2, td.dl-2 {
  background: @color_dl2_back@;
  }
.mOvr1, tr.mOvr1 {
  background: @color_mOver1_back@;
  cursor: pointer;
  }
.mOvr2, tr.mOvr2 {
  background: @color_mOver2_back@;
  cursor: pointer;
  }
.mOvr3, tr.mOvr3 {
  background: @color_mOver3_back@;
  cursor: pointer;
  }
table.uploaders, table.friends, table.bw_stats, table.vo, table.cs, table.servers,
table.shares, table.downloaders, table.scan_temp, table.upstats, table.messages,
table.shares, table.vc, table.results, table.networkInfo, table.memstats {
  margin-right: auto;
  margin-left: auto;
  border: @color_general_border@ solid 1px;
  border-collapse: collapse;
  }
table.sourcesInfo, table.subfilesInfo, table.serversC {
  width: 100%;
  margin-right: auto;
  margin-left: auto;
  border: @color_general_border@ solid 1px;
  border-collapse: collapse;
  }
table.sources {
  border: @color_general_border@ solid 1px;
  border-collapse: collapse;
  }
table.main {
  margin-right: auto;
  margin-left: auto;
  }
div.main, div.uploaders, div.friends, div.cs, div.shares, div.upstats, div.servers, div.serversC, div.vo,
div.downloaders, div.messages, div.vc, div.bw_stats, div.scan_temp, div.results, div.memstats {
  text-align: center;
  }
td.srb {
  padding-top: 1px;
  padding-bottom: 1px;
  font-size: 10px;
  font-family: Verdana;
  white-space: nowrap;
  border-right: @color_general_border@ solid 1px;
  border-bottom: @color_general_border@ solid 1px;
  border-left: @color_general_border@ solid 1px;
  border-top: @color_general_border@ solid 0px;
  padding-left: 3px;
  padding-right: 3px;
  color: @color_general_text@;
  }
td.act {
  font-size: 10px;
  font-weight: 700;
  }
td.br {
  border-right: @color_general_border@ dotted 1px;
  }
td.ar {
  text-align: right;
  }
td.al {
  text-align: left;
  }
td.ac {
  text-align: center;
  }
td.chunk0 {
  height: 12px;
  background: @color_chunk0@;
  }
td.chunk1 {
  height: 12px;
  background: @color_chunk1@;
  }
td.chunk2 {
  height: 12px;
  background: @color_chunk2@;
  }
td.chunk3 {
  height: 12px;
  background: @color_chunk3@;
  }
"

let html_js_mods0 = define_option message_section ["html_js_mods0"]
  "Main JS include style 0"
    string_option
"
<!--
var mOvrClass='';
function mOvr(src,clrOver) {
 if (clrOver == undefined) {var clrOver='mOvr1'};
 mOvrClass = src.className;
 src.className = clrOver + ' ' + mOvrClass + ' ' + clrOver;
}
function mOut(src) {
 src.className=mOvrClass;
}
function _getFrameByName(name) {
  switch (name) {
    case 'commands' :
      return('0');
    case 'fstatus' :
      return('1');
    case 'output' :
      return('2');
    default :
      alert('wrong arg for _getFrameByName');
      break;
  }
}
function mSub(target,cmd) {
  if (cmd=='kill') {
    if (confirm('Are you sure?')) {
      parent.frames[_getFrameByName(target)].location.href='submit?q=kill';
    }
  }
  else {
    if (cmd.substring(0,6)=='custom') {
      parent.frames[_getFrameByName(target)].location.href='submit?' + cmd;
    }
    else {
      parent.frames[_getFrameByName(target)].location.href='submit?q=' + cmd;
    } 
  }
}

function showTab(t){
       for (i=1; i<=6; i++) document.getElementById('tab' + i).style.display = 'none';
       document.getElementById(\"tab\" + t).style.display = 'block';
}
var _tabLast=null;
function _rObj (s,ar) {
 this.s = s;
 this.ar = ar;
}
// http://slayeroffice.com/code/functions/so_getText.html
function so_getText(obj) {
        if(obj.textContent) return obj.textContent;
        if (obj.nodeType == 3) return obj.data;
        var txt = new Array(), i=0;
        while(obj.childNodes[i]) {
                txt[txt.length] = so_getText(obj.childNodes[i]);
                i++;
        }
  return txt.join(\"\");
}
function _tabCreateArray(obj,st,total){
        var tb=obj.parentNode.parentNode;
        var rw=obj.parentNode.parentNode.rows;
        var _nRows=rw.length-total;
        var _tabS=new Array(_nRows-1);
        var _nCells = rw.item(0).cells.length;
        for(var i=1;i<_nRows;i++){
        var _raw = so_getText(rw.item(i).cells.item(obj.cellIndex)); //.innerHTML;
        if (st==1) {
            var _regexp = /[TGMk]$/;
            _raw = _raw.replace(/\\(/gi, \"\");
            if (_raw.indexOf(\":\") != -1) { _raw = _raw.substring(2,99); }
            if (_regexp.test(_raw)) {
              switch (_raw.charAt(_raw.search(_regexp))) {
               case \"k\": _raw = parseFloat(_raw) * 1024; break;
               case \"M\": _raw = parseFloat(_raw) * 1024 * 1024; break;
               case \"G\": _raw = parseFloat(_raw) * 1024 * 1024 * 1024; break;
               case \"T\": _raw = parseFloat(_raw) * 1024 * 1024 * 1024 * 1024; break;
               }
            }
        }
        _tabS[i-1]= new _rObj(_raw,rw.item(i).cloneNode(true));
        }
        if (st==1) { _tabS.sort(_cmpFloat); }
        else { _tabS.sort(_cmpTxt); }
        if (!_tabMode) {_tabS.reverse()}
        for(var i=0;i<_nRows-1;i++){
                        var tr = _tabS[i].ar.cloneNode(true);
                        var oChild=tb.rows.item(i+1);
                        if (i % 2 == 0) { tr.className = 'dl-1'; }
                               else { tr.className = 'dl-2'; }
                        tb.replaceChild(tr,oChild);
        }

}
function _cmpTxt(a,b) {
        if (_tabMode) {
                if (a.s==\"\") { if (b.s !=\"\") { return 1;} }
                if (b.s==\"\") { if (a.s !=\"\") { return -1;} }
        }
        if (a.s.toUpperCase() < b.s.toUpperCase()) {return -1;}
        if (a.s.toUpperCase() > b.s.toUpperCase()) {return 1;}
        return 0;
}
function _cmpFloat(a,b) {
        if (!_tabMode) {
                if (a.s==\"\") { if (b.s !=\"\") { return -1;} }
                if (b.s==\"\") { if (a.s !=\"\") { return 1;} }
        }
        if (isNaN(parseFloat(a.s))) {return 1;}
        if (isNaN(parseFloat(b.s))) {return -1;}
        return (parseFloat(b.s) - parseFloat(a.s));
}
function _tabSort(obj,st,total){
        if (_tabLast==obj) {_tabMode=!(_tabMode);}
        else {_tabMode=true;}
        _tabCreateArray(obj,st,total);
        _tabLast=obj;
        return _tabMode;
}
function _cmdLine(){
top.fstatus.document.open();
top.fstatus.document.clear();
top.fstatus.document.writeln(\"<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN' 'http://www.w3.org/TR/html4/loose.dtd'>\");
top.fstatus.document.writeln(\"<html><head>\");
top.fstatus.document.writeln(\"<link href='h.css' rel='stylesheet' type='text/css'>\");
top.fstatus.document.writeln(\"</head><body><center><table width=99% border=0 cellspacing=0 cellpadding=0>\");
top.fstatus.document.writeln(\"<form action=submit target=$O name=cmdFormular> \" );
top.fstatus.document.writeln(\"<tr><td width=100% nowrap>\");
top.fstatus.document.writeln(\" <input class='txt' style='width: 99%; height: 20px; font-size: 12px;\'\");
top.fstatus.document.writeln(\" type=text name=q value=''> </td><td width=1>\");
top.fstatus.document.writeln(\"	<input class='but' style='color: #FFF; font-weight: 600; height: 20px; font-size: 10px;'\");
top.fstatus.document.writeln(\"type=submit value=Execute></td></form>\");
top.fstatus.document.writeln(\"</tr></table></body></html>\");
top.fstatus.document.close();
}

if (document.layers) {navigator.family = \"nn4\"}
if (document.all) {navigator.family = \"ie4\"}
if (window.navigator.userAgent.toLowerCase().match(\"gecko\")) {navigator.family = \"gecko\"}

overdiv=\"0\";
function popLayer(a){
if (navigator.family == \"gecko\") {pad=\"0\"; bord=\"1 bordercolor=black\";}
else {pad=\"1\"; bord=\"0\";}
desc = \"<table cellspacing=0 cellpadding=\"+pad+\" border=\"+bord+\"  bgcolor=000000><tr><td>\\n\"
        +\"<table cellspacing=0 cellpadding=10 border=0 width=100%><tr><td class=popupstyle><center><font size=-1>\\n\"
        +a
        +\"\\n</td></tr></table>\\n\"
        +\"</td></tr></table>\";
if(navigator.family ==\"nn4\") {
        document.object1.document.write(desc);
        document.object1.document.close();
        document.object1.left=x+15;
        document.object1.top=y-5;
        }
else if(navigator.family ==\"ie4\"){
        object1.innerHTML=desc;
        object1.style.pixelLeft=x+15;
        object1.style.pixelTop=y-5;
        }
else if(navigator.family ==\"gecko\"){
        document.getElementById(\"object1\").innerHTML=desc;
        document.getElementById(\"object1\").style.left=x+15;
        document.getElementById(\"object1\").style.top=y-5;
        }
}

function hideLayer(){
if (overdiv == \"0\") {
        if(navigator.family ==\"nn4\") {eval(document.object1.top=\"-500\");}
        else if(navigator.family ==\"ie4\"){object1.innerHTML=\"\";}
        else if(navigator.family ==\"gecko\") {document.getElementById(\"object1\").style.top=\"-500\";}
        }
}

var isNav = (navigator.appName.indexOf(\"Netscape\") !=-1);
function handlerMM(e){

x = (isNav) ? e.pageX : event.clientX + document.body.scrollLeft;
y = (isNav) ? e.pageY : event.clientY + document.body.scrollTop;

}
if (isNav){document.captureEvents(Event.MOUSEMOVE);}
document.onmousemove = handlerMM;

function dllink() {
    var popw = 620;
    var poph = 330;
    var popx = 0;
    var popy = 0;
    popx = screen.availWidth/2 - popw/2;
    popy = screen.availHeight/2 - poph/2;
    var dimensions = 'height='+poph+', width='+popw+', top='+popy+', left='+popx+', scrollbars=yes, resizable=yes';
    window.open(\"multidllink.html\", \"_blank\", dimensions );
}

function servers() {
        var l = prompt( \"enter link to server.met for import\", \"\" );
        if( l != null ) {
                var f = document.forms[\"cmdFormular\"];
                var t = f.elements[\"q\"].value;
                f.elements[\"q\"].value = \"servers \" + l;
                f.submit();
                f.elements[\"q\"].value = t;
        }
}

function track_changed(obj)
{
  if (obj.value != obj.defaultValue)
    obj.className += \" changed\";
  else
    obj.className = obj.className.replace(/\\bchanged\\b/,'');
}

function xhr_ok_handler(f) {
 return function() {
// alert(this.readyState);
   if (this.readyState != 4) return;
   if (this.status == 200) { f(this.responseText); }
 }
}

function xhr_get(url,f) {
var client = new XMLHttpRequest();
client.onreadystatechange = xhr_ok_handler(f);
client.open(\"GET\", url);
client.send();
}

function toggle_priority(o,file,sub)
{
  return function(prio) {
    var p = \"0\";
    if (prio == \"0\") p = \"1\";
    o.onclick = function() {
      xhr_get(\"submit?api=set_subfile_prio+\"+file+\"+\"+p+\"+\"+sub, toggle_priority(o,file,sub)); };
    o.innerText = \"priority \"+prio;
  }
}

//-->
  "

let multidllink_old =  define_option message_section ["multidllink_old"]
  "multidllink - old"
  string_option
  "OLD multidllink JAVE"

let multidllink_mods0 =  define_option message_section ["multidllink_mods0"]
  "multidllink - Style 0"
  string_option
  "
<html>
<head>
<link href=\"h.css\" rel=\"stylesheet\" type=\"text/css\" />
</head>
<script type=\"text/javascript\">
window.document.title='Insert links here and press Input';

function fixHeightOfTheText()
{
  var t = document.getElementById(\"links\");

  var myWidth = 0, myHeight = 0;
  if( typeof( window.innerWidth ) == 'number' ) {
    //Non-IE
    myWidth = window.innerWidth;
    myHeight = window.innerHeight;
  } else if( document.documentElement && ( document.documentElement.clientWidth || document.documentElement.clientHeight ) ) {
    //IE 6+ in 'standards compliant mode'
    myWidth = document.documentElement.clientWidth;
    myHeight = document.documentElement.clientHeight;
  } else if( document.body && ( document.body.clientWidth || document.body.clientHeight ) ) {
    //IE 4 compatible
    myWidth = document.body.clientWidth;
    myHeight = document.body.clientHeight;
  }

  h=myHeight;

  var height = (h - t.offsetTop - 45) + \"px\";

  t.style.height = height;

}
window.onresize = fixHeightOfTheText;


</script>
<body onload=\"document.dllinkform.links.focus();\">
<form name=\"dllinkform\" method=\"GET\" action=\"submit\">
<table width=\"100%\">
<input type=\"hidden\" name=\"jvcmd\" value=\"multidllink\">
<tr><td><textarea id=\"links\" style=\"width: 100%; height:100%\" name=\"links\"></textarea> <br></td></tr>
<tr><td align=\"right\" class=\"bu bbigm\" style=\"padding-top: 0px; padding-bottom: 0px;\" title=\"Download Links\"  ><INPUT class=\"but2\" type=submit value=\"Input\" style=\"width:100%\"></td></tr>
</form>
<script type=\"text/javascript\">
fixHeightOfTheText(); 
</script>
</body>
</html>
"


let html_header_mods0 = define_option message_section ["html_header_mods0"]
  "Header - style 0"
    string_option
("<title>" ^ _s "MLdonkey: Web Interface" ^ "</title>
<meta name=\"generator\" content=\"MLDonkey\" >
<meta name=\"robots\" content=\"noindex,nofollow\" >
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" >
<meta http-equiv=\"Expires\" content=\"-1\" >
<meta http-equiv=\"Pragma\" content=\"no-cache\" >
<link rel=\"shortcut icon\" href=\"favicon.ico\" type=\"image/x-icon\" >
<link href=\"h.css\" rel=\"stylesheet\" type=\"text/css\" >
<script type=\"text/javascript\" src=\"i.js\" >
</script>")

let download_html_css_mods0 = define_option message_section ["download_html_css_mods0"]
  "Download CSS - style 0"
    string_option
"
body {
  background-color: @color_vd_page_background@;
  color: @color_general_text@;
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 13px;
  margin-top: 10px;
  margin: 2;
  }
td, pre {
  color: @color_general_text@;
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 10px;
  }
table.downloaders {
  margin-right: auto;
  margin-left: auto;
  border: @color_general_border@ solid 1px;
  }
div.main {
  text-align: center;
  }
table.main {
  margin-right: auto;
  margin-left: auto;
  }
td.loaded {
  padding-top: 0px;
  padding-bottom: 0px;
  background-color: @color_vd_downloaded@;
  font-size: 1px;
  line-height: 2px;
  }
td.remain {
  padding-top: 0px;
  padding-bottom: 0px;
  background-color: @color_vd_remaining@;
  font-size: 1px;
  line-height: 2px;
  }
td.downloaded {
  font-family: Verdana;
  font-weight: 500;
  font-size: 12px;
  color: @color_general_text@;
  }
td.dl {
  white-space: nowrap;
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  font-family: verdana;
  font-size: 10px;
  color: @color_general_text@;
  }
td.dlheader {
  cursor: pointer;
  color: @color_general_text@;
  font-family: Verdana, serif;
  font-size: 10px;
  border-bottom: solid 1px;
  background: @color_table_header_background@;
  padding-left: 3px;
  padding-right: 3px;
  }
input.checkbox {
  background: @color_table_header_background@;
  vertical-align: middle;
  height: 10px;
  width: 10px;
  }
td.sr {
  white-space: nowrap;
  padding-top: 2px;
  padding-bottom: 2px;
  padding-left: 5px;
  padding-right: 5px;
  font-family: verdana;
  font-size: 10px;
  color: @color_general_text@;
  }
table {
  border-spacing: 0px;
  }
td {
  padding: 0px;
  }
td.ar {
  text-align: right;
  }
td.al {
  text-align: left;
  }
td.ac {
  text-align: center;
  }
td.brs {
  border-right: @color_general_border@ solid 1px;
  padding-left: 2px;
  padding-right: 2px;
  text-align: center;
  }
td.np {
  padding-left: 2px;
  padding-right: 0px;
  text-align: center;
  }
td.big {
  border-top: @color_general_border@ solid 1px;
  border-left: @color_general_border@ solid 1px;
  }
td.pr {
  border-right: @color_general_border@ solid 1px;
  }
.bigbutton {
  color: @color_general_text@;
  font-family: Verdana, serif;
  font-size: 10px;
  background: @color_background@;
  border: @color_background@ solid 1px;
  cursor: pointer;
  }
.headbutton {
  font-family: Verdana, serif;
  font-size: 10px;
  border: @color_table_header_background@ solid 1px;
  background: @color_table_header_background@;
  padding-left: 5px;
  padding-right: 5px;
  cursor: pointer;
  color: @color_general_text@;
  }
tr.dl-1 {
  background: @color_dl1_back@;
  }
tr.dl-2 {
  background: @color_dl2_back@;
  }
tr.mOvrDL, .mOvrDL {
  background: @color_mOver1_back@;
  cursor: pointer;
  }
input {
  font-family: tahoma;
  font-size: 10px;
  }
a {
  text-decoration: none;
  font-weight: bold;
  }
a:link,a:active,a:visited {
  color: @color_download_anchor@;
  }
a:hover {
  color: @color_download_anchor_hover@;
  text-decoration: underline;
  }
a.extern:visited,a.extern:hover,a.extern:active {
  color: @color_external_anchor@;
  }
.extern:hover {
  color: @color_external_anchor_hover@;
  }
"

let download_html_js_mods0 = define_option message_section ["download_html_js_mods0"]
  "Download JS include style 0"
    string_option
"
<!--
var mOvrClass='';
function mOvr(src,clrOver) {
 if (clrOver == undefined) {var clrOver='mOvrDL'};
 mOvrClass = src.className;
 src.className = mOvrClass + ' ' + clrOver;
}
function mOut(src) {
 src.className=mOvrClass;
}

overdiv=\"0\";
function popLayer(a){
if (navigator.family == \"gecko\") {pad=\"0\"; bord=\"1 bordercolor=black\";}
else {pad=\"1\"; bord=\"0\";}
desc = \"<table cellspacing=0 cellpadding=\"+pad+\" border=\"+bord+\"  bgcolor=000000><tr><td>\\n\"
        +\"<table cellspacing=0 cellpadding=10 border=0 width=100%><tr><td bgcolor=#C1CADE><center><font size=-1>\\n\"
        +a
        +\"\\n</td></tr></table>\\n\"
        +\"</td></tr></table>\";
if(navigator.family ==\"nn4\") {
        document.object1.document.write(desc);
        document.object1.document.close();
        document.object1.left=x+15;
        document.object1.top=y-5;
        }
else if(navigator.family ==\"ie4\"){
        object1.innerHTML=desc;
        object1.style.pixelLeft=x+15;
        object1.style.pixelTop=y-5;
        }
else if(navigator.family ==\"gecko\"){
        document.getElementById(\"object1\").innerHTML=desc;
        document.getElementById(\"object1\").style.left=x+15;
        document.getElementById(\"object1\").style.top=y-5;
        }
}

function hideLayer(){
if (overdiv == \"0\") {
        if(navigator.family ==\"nn4\") {eval(document.object1.top=\"-500\");}
        else if(navigator.family ==\"ie4\"){object1.innerHTML=\"\";}
        else if(navigator.family ==\"gecko\") {document.getElementById(\"object1\").style.top=\"-500\";}
        }
}

var isNav = (navigator.appName.indexOf(\"Netscape\") !=-1);
function handlerMM(e){

x = (isNav) ? e.pageX : event.clientX + document.body.scrollLeft;
y = (isNav) ? e.pageY : event.clientY + document.body.scrollTop;

}
if (isNav){document.captureEvents(Event.MOUSEMOVE);}
document.onmousemove = handlerMM;
//-->
"

let download_html_header_mods0 = define_option message_section ["download_html_header_mods0"]
  "Download header - style 0"
    string_option
  ("
<title>" ^ _s "MLdonkey: Web Interface" ^ "</title>
<link href=\"dh.css\" rel=\"stylesheet\" type=\"text/css\">
<script type=\"text/javascript\" src=\"di.js\"></script>
  ")

let web_common_header_mods0 = define_option message_section ["web_common_header_mods0"]
  "Web header - style 0"
    string_option
("
<!-- Main Table -->
<FORM name=cmdFormular action=submit target=output>
<TABLE BORDER=0 cellspacing=1 cellpadding=0 width=\"100%\"><TR>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\" " ^ _s "Transfers Tab" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(1);mSub('fstatus','bw_stats');mSub('output','vd');\">" ^ _s
"Transfers" ^ "</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\" " ^ _s "Searches Tab" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(2);mSub('fstatus','view_custom_queries');mSub('output','custom=Complex+Search');\">" ^ _s
"Search" ^ "</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\" " ^ _s "Servers Tab" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(3);mSub('fstatus','bw_stats');mSub('output','vm');\">" ^ _s
"Servers" ^ "</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\" " ^ _s "Statistics Tab" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(4);mSub('fstatus','bw_stats');mSub('output','stats');\">" ^ _s
"Statistics" ^ "</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\" " ^ _s "Options Tab" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(5);mSub('fstatus','version');mSub('output','voo+1');\">" ^ _s
"Options" ^ "</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\" " ^ _s "Help+Miscellaneous Tab" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(6);mSub('fstatus','version');mSub('output','help');\">" ^ _s
"Help+" ^ "</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\" " ^ _s "dllink" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"dllink();\">" ^ _s
"DL" ^ "</TD></TR></TBODY></TABLE></TD>
<TD noWrap width=\"100%\" title=\" " ^ _s "Input mldonkey commands here" ^ " \">
<TABLE cellSpacing=0 cellpadding=0 width=\"100%\"><TBODY><TR>
<TD style=\"height: 1%; padding: 0px; border: 0px; padding-left: 5px;\" title=\" " ^ _s "Input mldonkey command here" ^ " \">
<INPUT class=\"txt2\" style=\"WIDTH: 99%;\" name=q>
</TD></TR></TBODY></TABLE></TD><TD noWrap>
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\"><TBODY><TR>
<TD class=\"bu bbigm\" style=\"padding-top: 0px; padding-bottom: 0px;\" title=\" " ^ _s "Input Command" ^ " \">
<INPUT class=\"but2\" type=submit value=\" " ^ _s "Input" ^ " \">
</TD></TR></TBODY></TABLE>
</TD>
</TR></TABLE>
<!-- End Main Table -->

<!-- set focus -->
<script type=\"text/javascript\">
document.forms['cmdFormular'].elements['q'].focus();
</script>

<DIV ID=\"tab1\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\" " ^ _s "Current downloads" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','vd')\">" ^ _s
"Downloads" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Current downloaders" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','downloaders')\">" ^ _s
"Downloaders" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Upload statistics" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','upstats')\">" ^ _s
"Uploads" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Uploaders" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','uploaders')\">" ^ _s
"Uploaders" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Commit downloaded files to incoming directory" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','commit')\">" ^ _s
"Commit" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Check shared files for removal" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','reshare')\">" ^ _s
"Reshare" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "List contents of the temp directory" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','scan_temp')\">" ^ _s
"Scan temp" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Bandwidth statistics (set html_mods_bw_refresh_delay)" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','gdstats')\">" ^ _s
"Bandwidth stats" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Toggle bandwidth and max connections" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','bw_toggle')\">" ^ _s
"Bandwidth toggle" ^ "</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab2\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\" " ^ _s "Extend search to more servers and view results" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','xs');mSub('output','vr');\">" ^ _s
"Extend search" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "View search results" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','view_custom_queries');mSub('output','vr')\">" ^ _s
"Search results" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "View searches" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','view_custom_queries');mSub('output','vs')\">" ^ _s
"View searches" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Complex search" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','view_custom_queries');mSub('output','custom=Complex+Search')\">" ^ _s
"Complex search" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "MP3 search" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','view_custom_queries');mSub('output','custom=MP3+Search')\">" ^ _s
"MP3 search" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Movie search" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','view_custom_queries');mSub('output','custom=Movie+Search')\">" ^ _s
"Movie search" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Album search" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','view_custom_queries');mSub('output','custom=Album+Search')\">" ^ _s
"Album search" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Force download (click after trying to download the duplicate file)" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','force_download')\">" ^ _s
"Force DL" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "View RSS feeds" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','view_custom_queries');mSub('output','rss')\">" ^ _s
"RSS" ^ "</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab3\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\" " ^ _s "List connected servers" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','vm')\">" ^ _s
"Connected servers" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "List all servers" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','vma')\">" ^ _s
"All servers" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Connect to more servers" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','c')\">" ^ _s
"Connect to more servers" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Remove old servers" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','remove_old_servers')\">" ^ _s
"Remove old servers" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Import Serverlist" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"servers();\">" ^ _s
"Import Server.met" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Open Serverlist" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');parent.frames[_getFrameByName('output')].location.href='http://www.gruk.org/list.php'\">" ^ _s
"Serverlist" ^ "</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab4\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\" " ^ _s "eDonkey statistics in a table" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','cs')\">" ^ _s
"eDonkey Table" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "eMule MODs statistics" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','csm')\">" ^ _s
"eMule MODs" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Bittorrent statistics" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','csbt')\">" ^ _s
"Bittorrent Table" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Overnet statistics" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','ov_view_stats_cmds');mSub('output','ov_stats')\">" ^ _s
"Overnet" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Kademlia statistics" ^ " \" 
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','kad_view_stats_cmds');mSub('output','kad_stats')\">" ^ _s
"Kademlia" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Gnutella statistics" ^ " \" 
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','gstats')\">" ^ _s
"Gnutella" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Gnutella2 statistics" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','g2stats')\">" ^ _s
"Gnutella2" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Country statistics - all seen" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','costats all')\">" ^ _s
"Countries" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Memory statistics" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','mem_stats 0')\">" ^ _s
"Memory" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Sources statistics" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats');mSub('output','sources')\">" ^ _s
"Sources" ^ "</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab5\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\" " ^ _s "Settings" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','version');mSub('output','voo+1')\">" ^ _s
"Settings" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Users" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"mSub('fstatus','version');mSub('output','users')\">" ^ _s
"Users" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "View/edit shared directories" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','version');mSub('output','shares')\">" ^ _s
"Shares" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Friends" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','version');mSub('output','friends')\">" ^ _s
"Friends" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "View/send messages (20 second refresh)" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','version');mSub('output','message')\">" ^ _s
"Messages" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "IP blocking statistics" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','version');mSub('output','block_list')\">" ^ _s
"IP blocking" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Recover files from temp directory" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','recover_temp');mSub('output','scan_temp');\">" ^ _s
"Recover temp" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Close all files (use to free space on disk after remove)" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','version');mSub('fstatus','close_fds')\">" ^ _s
"Close files" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "View all clients" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','version');mSub('output','vc+all')\">" ^ _s
"View clients" ^ "</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab6\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\" " ^ _s "Long help" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','longhelp')\">" ^ _s
"LongHelp" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Network listing" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','networks')\">" ^ _s
"Networks" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Sysinfo" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','sysinfo')\">" ^ _s
"Sysinfo" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Porttest" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','porttest')\">" ^ _s
"Porttest" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "View ChangeLog" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"parent.frames[_getFrameByName('output')].location.href='https://raw.githubusercontent.com/ygrek/mldonkey/master/CHANGES'\">" ^ _s
"ChangeLog" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Homepage" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"parent.frames[_getFrameByName('output')].location.href='http://mldonkey.sourceforge.net'\">" ^ _s
"Homepage" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Support forum" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"parent.frames[_getFrameByName('output')].location.href='http://mldonkey.sourceforge.net/forums'\">" ^ _s
"English support" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "View core log" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','log')\">" ^ _s
"Log" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Logout interface" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','logout')\">" ^ _s
"Logout" ^ "</TD>
<TD class=\"bu bbig\" title=\" " ^ _s "Kill core" ^ " \"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','kill')\">" ^ _s
"Kill core" ^ "</TD>
</TR></TBODY></TABLE></DIV>
</FORM>
")

(* Old *)

let html_css_old = define_option message_section
  ["html_css_old"]
  "The old css"
    string_option
  "
body,th,td { background-color:#EAE8CF;color: #3F702E; font-family: Verdana, Arial, Helvetica, sans-serif;font-size: 12px; }
a { text-decoration: none; }
a:hover { text-decoration: underline; color: #660000; }
a:link,a:active,a:visited { color: #660000; }
a.extern:visited,a.extern:active { color: #000099; }
a.extern:hover { color: #000000; }
  "

let html_js_old = define_option message_section
  ["html_js_old"]
  "The old js"
    string_option
  "
<!--
function CheckInput(){
var cmdString = document.cmdFormular.q.value;
return true;
}
//-->
  "

let html_header_old = define_option message_section ["html_header_old"]
  "The header used in the WEB interface (modify to add your CSS)"
    string_option
  "<title>MLDonkey: Web Interface</title>
<link href=\"h.css\" rel=\"stylesheet\" type=\"text/css\">
<script type=\"text/javascript\" src=\"i.js\"></script>
    "

let download_html_css_old = define_option message_section ["download_html_css_old"]
  "The small CSS)"
    string_option
  "
body { background-color: #EAE8CF; color: #3F702E; font-family: Verdana, Arial, Helvetica, sans-serif; font-size: 12px; margin: 2; }
td,pre {color: #3F702E; font-family: Verdana, Arial, Helvetica, sans-serif; font-size: 12px; }

td.loaded { background-color: #6666cc; font-size: 2px; }
td.remain { background-color: #cc6666; font-size: 2px; }

a { text-decoration: none; font-weight: bold; }
a:link,a:active,a:visited { color: #882222; }
a:hover { color: #000000; text-decoration: underline;}

a.extern:visited,a.extern:active { color: #000099; }
a.extern:hover { color: #000000; }
  "

let download_html_js_old = define_option message_section ["download_html_js_old"]
  "The old js"
    string_option
"
<!--
//-->
  "

let download_html_header_old = define_option message_section ["download_html_header_old"]
  "The header used in the WEB interface for downloads (modify to add your CSS)"
    string_option
  "
<title>MLdonkey: Web Interface</title>
<link href=\"dh.css\" rel=\"stylesheet\" type=\"text/css\">
<script type=\"text/javascript\" src=\"di.js\"></script>
"

let web_common_header_old = define_option message_section ["web_common_header_old"]
  "The header displayed in the WEB interface"
    string_option
  "
<table width=\"100%\" border=\"0\">
<tr>
<td align=\"left\" valign=\"middle\" width=\"*\"><a href=\"http://mldonkey.sf.net/\" $O><b>MLDonkey Home</b></a></td>
<form action=\"submit\" $O name=\"cmdFormular\" onSubmit=\"return CheckInput();\">
<td><input type=\"text\" name=\"q\" size=60 value=\"\"></td>
<td><input type=\"submit\" value=\"Execute\"></td>
</form>
</tr>
</table>
<table width=\"100%\" border=\"0\">
<tr>
<td><a href=\"files\" onMouseOver=\"window.status='View current downloads status';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Downloads</a></td>
<td><a href=\"submit?q=view_custom_queries\" onMouseOver=\"window.status='Send a customized query';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Custom search</a></td>
<td><a href=\"submit?q=vm\" onMouseOver=\"window.status='View current connection status';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Connected servers</a></td>
<td><a href=\"submit?q=help\" onMouseOver=\"window.status='View a list of all available commands';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Help</a></td>
<td><a href=\"submit?q=vo\" onMouseOver=\"window.status='View and change your preferences';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Preferences</a></td>
</tr>
<tr>
<td><a href=\"submit?q=upstats\" onMouseOver=\"window.status='View current upload status';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Uploads</a></td>
<td><a href=\"submit?q=xs\" onMouseOver=\"window.status='Extend your search to more servers';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Extend search</a></td>
<td><a href=\"submit?q=c\" onMouseOver=\"window.status='Connect to more servers';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Connect more servers</a></td>
<td><a href=\"submit?q=version\" onMouseOver=\"window.status='Check version of MLDonkey';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Version</a></td>
<td><a href=\"submit?q=remove_old_servers\" onMouseOver=\"window.status='Remove servers that have not been connected for several days';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Clean old servers</a></td>
</tr>
<tr>
<td><a href=\"submit?q=uploaders\" onMouseOver=\"window.status='View a list of your upload slots';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Upload slots</a></td>
<td><a href=\"submit?q=vs\" onMouseOver=\"window.status='View a list of your sent queries';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>View searches</a></td>
<td><a href=\"submit?q=vma\" onMouseOver=\"window.status='View a list of all known servers';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>View all servers</a></td>
<td><a href=\"submit?q=client_stats\" onMouseOver=\"window.status='Gives stats about your transfers';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Client stats</a></td>
<td><a href=\"submit?q=reshare\" onMouseOver=\"window.status='Check shared files for removal';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Reshare files</a></td>
<td><a href=\"submit?q=html_mods\" onMouseOver=\"window.status='Toggle html_mods';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Toggle html_mods</a></td>
</tr>
<tr>
<td><a href=\"submit?q=commit\" onMouseOver=\"window.status='Move finished downloads to incoming directory';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Commit</a></td>
<td><a href=\"submit?q=vr\" onMouseOver=\"window.status='View results to your queries';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Search results</a></td>
<td><a href=\"submit?q=ovweb\" onMouseOver=\"window.status='Boot Overnet peers from http list';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Load Overnet peers</a></td>
<td><a class=\"extern\" href=\"http://mldonkey.sf.net/forums/\" onMouseOver=\"window.status='MLDonkey World';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>English forum</a></td>
<td><a class=\"extern\" href=\"http://mldonkey.sf.net/\" onMouseOver=\"window.status='German Forum';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>German forum</a></td>
<td><a href=\"submit?q=kill\" onMouseOver=\"window.status='Save and quit MLDonkey';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Kill MLDonkey</a></td>
  </tr>
  </table>
"

let available_commands_are = _s  "Available commands are:\n"

let main_commands_are = _s  "Main commands are:\n"

let command_not_authorized = _s "Command not authorized\n use 'auth <user> <password>' before."

let bad_login = _s  "Bad login/password"

let full_access = _s "Full access enabled"

let download_started = message "download_started"
    (T.boption (T.int T.bformat)) "Download of file %d started<br>"

let no_such_command  = message "no_such_command"
    (T.boption (T.string T.bformat))   "No such command %s\n"

let bad_number_of_args cmd help = _s (Printf.sprintf "Bad number of arguments, see help for correct use:\n%s %s" cmd help)

(* Colour arrays list for debug and devs

-- Isn't that redundant, hence, probably already out of sync ?

Main CSS :

body { background: @color_background@;    
scrollbar-face-color: @color_scrollbar_face@; scrollbar-shadow-color: @color_scrollbar_face@;
scrollbar-highlight-color: @color_scrollbar_highlight@; scrollbar-3dlight-color: @color_some_scrollbar@;
scrollbar-darkshadow-color: @color_some_scrollbar@; scrollbar-track-color: @color_background@;
scrollbar-arrow-color: @color_some_scrollbar@; }
table.commands { border: @color_general_border@; background: @color_background@;}
table.topcommands { background: @color_background@; border: @color_general_border@; border-top: @color_scrollbar_highlight@; border-left: @color_scrollbar_highlight@;} 
pre { color: @color_general_text@; }
p { color: @color_general_text@; }
input.txt { background: @color_input_text@; }
input.txt2 { background: @color_bbig_background@; color: @color_general_text@;
border-right: @color_some_border@; border-top: @color_general_border@; border-left: @color_general_border@; border-bottom: @color_some_border@; }
input.but2 { background: @color_bsmall3@; }
input.but { background: @color_input_button@; }
a:link,a:active,a:visited { color: @color_anchor@; }
a:hover { color: @color_anchor_hover@; }
.bu { background: @color_chunk3@; color: @color_foreground_text_for_top_buttons@; border: @color_some_border@; }
.bbig { border-top: @color_scrollbar_highlight@;  border-left: @color_scrollbar_highlight@;  border-bottom: @color_general_border@;  border-right: @color_general_border@;
color: @color_general_text@; background: @color_bbig_background@; }
.bbigm { border-top: @color_scrollbar_highlight@; border-left: @color_scrollbar_highlight@; border-bottom: @color_general_border@; border-right: @color_general_border@;
color: @color_general_text@; background: @color_bsmall3@; }
.bsmall { background: @color_bsmall_back@; }
.bsmall1 { background: @color_bbig_background@; }
.bsmall2 { background: @color_bsmall2@; }
.bsmall3 { background: @color_bsmall3@; }
.bbig2 { background: @color_bsmall3@; }
.bbig3 { background: @color_scrollbar_face@; }
.b1 { border-left: @color_border_of_top_buttons@; border-top: @color_border_of_top_buttons@; border-right: @color_border_of_top_buttons@; border-bottom: @color_border_of_top_buttons@; }
.b2 { border-left: @color_border_of_top_buttons@; border-top: @color_border_of_top_buttons@; border-right: @color_border_of_top_buttons@; border-bottom: @color_border_of_top_buttons@; }
.b3 { border-left: @color_border_of_top_buttons@; border-top: @color_border_of_top_buttons@; border-right: @color_border_of_top_buttons@; border-bottom: @color_border_of_top_buttons@; }
.b4 { border-left: @color_border_of_top_buttons@; border-top: @color_border_of_top_buttons@; border-right: @color_border_of_top_buttons@; border-bottom: @color_border_of_top_buttons@; }
.bb1 { border-left: @color_general_border@; border-top: @color_scrollbar_highlight@; border-right: @color_scrollbar_highlight@; border-bottom: @color_general_border@; }
.bb2 { border-left: @color_big_buttons_and_border_highlight@; border-top: @color_scrollbar_highlight@; border-right: @color_scrollbar_highlight@; border-bottom: @color_general_border@; }
.bb3 { border-left: @color_big_buttons_and_border_highlight@; border-top: @color_scrollbar_highlight@; border-right: @color_general_border@; border-bottom: @color_general_border@; }
.bb4 { border-left: @color_big_buttons_and_border_highlight@; border-top: @color_scrollbar_highlight@; border-right: @color_general_border@; border-bottom: @color_general_border@; }
.src { border-left: @color_general_border@; border-top: @color_general_border@; border-right: @color_general_border@; border-bottom: @color_general_border@; }
td.fbig { background: @color_fbig_background@; border-top: @color_general_border@; border-left: @color_general_border@; }
td.pr { border-right: @color_general_border@; }
td.fbigb { border-top: @color_general_border@; border-bottom: @color_general_border@; }
td.sr { color: @color_general_text@; }
td.srp { color: @color_one_td_text@; }
td.srw { color: @color_general_text@; }
td.srh { vertical-align: top; background: @color_table_header_background@; color: @color_general_text@; }
tr.dl-1, td.dl-1 { background: @color_dl1_back@; }
tr.dl-2, td.dl-2 { background: @color_dl2_back@; }
.mOvr1, tr.mOvr1 {background: @color_mOver1_back@; }
.mOvr2, tr.mOvr2 {background: @color_mOver2_back@; }
.mOvr3, tr.mOvr3 {background: @color_mOver3_back@; }
table.uploaders, table.friends, table.bw_stats, table.vo, table.cs, table.servers,
table.shares, table.downloaders, table.scan_temp, table.upstats, table.messages,
table.shares, table.vc, table.results, table.networkInfo { border: @color_general_border@; }
table.sourcesInfo, table.subfilesInfo, table.serversC { border: @color_general_border@; }
table.sources { border: @color_general_border@;}
td.srb { border-right: @color_general_border@; border-bottom: @color_general_border@;
border-left: @color_general_border@; border-top: @color_general_border@; }
td.br { border-right: @color_general_border@;}
.chunk0;{ background: @color_chunk0@}
.chunk1 { background: @color_chunk1@}
.chunk2 { background: @color_chunk2@}
.chunk3 { background: @color_chunk3@}

Download CSS
body{ background-color:@color_vd_page_background@; color: @color_general_text@; }
td,pre { color: @color_general_text@; }
table.downloaders { border: @color_general_border@;}
td.loaded{ background-color:@color_vd_downloaded@; }
td.remain{ background-color:@color_vd_remaining@; }
td.downloaded{ color: @color_general_text@;}
td.dl { color: @color_general_text@;  }
td.dlheader { color: @color_general_text@;  
border-bottom: ( background: @color_table_header_background@; }
input.checkbox { background: @color_table_header_background@; }
td.sr { color: @color_general_text@; }
td.brs { border-right: @color_general_border@; }
td.big { border-top: @color_general_border@;  border-left: @color_general_border@; }
td.pr { border-right: @color_general_border@; }
.bigbutton { background: @color_background@; border: @color_background@; color: @color_general_text@; }
.headbutton { border: @color_table_header_background@; background: @color_table_header_background@; }
tr.dl-1 { background: @color_dl1_back@; }
tr.dl-2 { background: @color_dl2_back@; }
tr.mOvrDL, .mOvrDL { background: @color_mOver1_back@;  }
a:link,a:active,a:visited { color: @color_download_anchor@; }
a:hover { color: @color_download_anchor_hover@; }
a.extern:visited,a.extern:hover,a.extern:active { color: @color_external_anchor@; }
.extern:hover { color: @color_external_anchor_hover@; }
*)

type style_type = {
  style_name: string;
  color_background: string;
  color_scrollbar_face: string;
  color_chunk2: string;
  color_scrollbar_highlight: string;
  color_vd_page_background: string;
  color_big_buttons_and_border_highlight: string;
  color_input_text: string;
  color_input_button: string;
  color_chunk3: string;
  color_foreground_text_for_top_buttons: string;
  color_fbig_background: string; (* (tabs) *)
  color_bbig_background: string; (* (vma button) *)
  color_bsmall_back: string; (* (help!) *)
  color_bsmall2: string; (* (options, memstats) *)
  color_bsmall3: string; (* (load onet peers) *)
  color_border_of_top_buttons: string;
  color_table_header_background: string;
  color_mOver1_back: string;
  color_mOver2_back: string;
  color_mOver3_back: string;
  color_dl1_back: string;
  color_dl2_back: string;
  color_chunk0: string;
  color_chunk1: string;
  color_vd_downloaded: string;
  color_vd_remaining: string;
  color_general_text: string;
  color_general_border: string;
  color_anchor: string;
  color_anchor_hover: string;
  color_download_anchor: string;
  color_download_anchor_hover: string;
  color_external_anchor: string;
  color_external_anchor_hover: string;
  color_some_scrollbar: string;
  color_some_border: string;
  color_one_td_text: string;
  color_header2_text: string;
  color_header3_text: string;
  color_background_popup: string;
  frame_height: int
}

let dummy_style = {
  style_name = "Dummy";
  color_background = "#000";
  color_scrollbar_face = "#000";
  color_chunk2 = "#000";
  color_scrollbar_highlight = "#000";
  color_vd_page_background = "#000";
  color_big_buttons_and_border_highlight = "#000";
  color_input_text = "#000";
  color_input_button = "#000";
  color_chunk3 = "#000";
  color_foreground_text_for_top_buttons = "#000";
  color_fbig_background = "#000";
  color_bbig_background = "#000";
  color_bsmall_back = "#000";
  color_bsmall2 = "#000";
  color_bsmall3 = "#000";
  color_border_of_top_buttons = "#000";
  color_table_header_background = "#000";
  color_mOver1_back = "#000";
  color_mOver2_back = "#000";
  color_mOver3_back = "#000";
  color_dl1_back = "#000";
  color_dl2_back = "#000";
  color_chunk0 = "#000";
  color_chunk1 = "#000";
  color_vd_downloaded = "#000";
  color_vd_remaining = "#000";
  color_general_text = "#000";
  color_general_border = "#000";
  color_anchor = "#0000ff";
  color_anchor_hover = "#0000ff";
  color_download_anchor = "#000";
  color_download_anchor_hover = "#000";
  color_external_anchor = "#000";
  color_external_anchor_hover = "#000";
  color_some_scrollbar = "#000";
  color_some_border = "#000";
  color_one_td_text = "#000";
  color_header2_text = "#000";
  color_header3_text = "#000";
  color_background_popup = "#000";
  frame_height = 0;
}

    (* Default *)
let styles = Array.of_list [
  { style_name = "Green";
    color_background = "#CBE5CB"; 
    color_scrollbar_face = "#94AE94";  
    color_chunk2 = "#33F"; 
    color_scrollbar_highlight = "#E5FFE5"; 
    color_vd_page_background = "#B2CCB2"; 
    color_big_buttons_and_border_highlight = "#E5E5E5"; 
    color_input_text = "#BADEBA"; 
    color_input_button = "#A3BDA3"; 
    color_chunk3 = "#00F"; 
    color_foreground_text_for_top_buttons = "#3D3D3D"; 
    color_fbig_background = "#86BE86"; 
    color_bbig_background = "#B2CCB2"; 
    color_bsmall_back = "#BCD6BC"; 
    color_bsmall2 = "#A8C2A8"; 
    color_bsmall3 = "#A3BDA3"; 
    color_border_of_top_buttons = "#718B71"; 
    color_table_header_background = "#90C890"; 
    color_mOver1_back = "#BADEBA"; 
    color_mOver2_back = "#F00"; 
    color_mOver3_back = "#94AE94";
    color_dl1_back = "#FFF"; 
    color_dl2_back = "#EEE"; 
    color_chunk0 = "#F33"; 
    color_chunk1 = "#101077" ; 
    color_vd_downloaded = "#72AA72"; 
    color_vd_remaining = "#EEE"; 
    color_general_text = "#000"; 
    color_general_border = "#000"; 
    color_anchor = "#0000ff"; 
    color_anchor_hover = "#0000ff"; 
    color_download_anchor = "#000"; 
    color_download_anchor_hover = "#000";
    color_external_anchor = "#000"; 
    color_external_anchor_hover = "#000099"; 
    color_some_scrollbar = "#000"; 
    color_some_border = "#FFF"; 
    color_one_td_text = "#555";
    color_header2_text = "#000";
    color_header3_text = "#000";
    color_background_popup = "#C1CADE";
    frame_height = 46; };
  
  { style_name = "Orange Tang";
    color_background = "#EEE"; 
    color_scrollbar_face = "#FF8800";  
    color_chunk2 = "#FF8800"; 
    color_scrollbar_highlight = "#FAD08F"; 
    color_vd_page_background = "#F5F5F5";
    color_big_buttons_and_border_highlight = "#F2C074"; 
    color_input_text = "#FF9900"; 
    color_input_button = "#FF9900"; 
    color_chunk3 = "#FF7700"; 
    color_foreground_text_for_top_buttons = "#FFF";
    color_fbig_background = "#FF9D2D"; 
    color_bbig_background = "#FF8800"; 
    color_bsmall_back = "#FF8800"; 
    color_bsmall2 = "#FF8800"; 
    color_bsmall3 = "#FF8800";
    color_border_of_top_buttons = "#FF9900"; 
    color_table_header_background = "#FF8800"; 
    color_mOver1_back = "#FACF8E"; 
    color_mOver2_back = "#FACF8E"; 
    color_mOver3_back = "#FACF8E";
    color_dl1_back = "#FFF"; 
    color_dl2_back = "#EEE"; 
    color_chunk0 = "#DDD"; 
    color_chunk1 = "#FACD88" ; 
    color_vd_downloaded = "#FF9900"; 
    color_vd_remaining = "#EEE";
    color_general_text = "#000"; 
    color_general_border = "#000"; 
    color_anchor = "#0000ff"; 
    color_anchor_hover = "#0000ff"; 
    color_download_anchor = "#000"; 
    color_download_anchor_hover = "#000";
    color_external_anchor = "#000"; 
    color_external_anchor_hover = "#000099"; 
    color_some_scrollbar = "#000"; 
    color_some_border = "#FFF"; 
    color_one_td_text = "#555";
    color_header2_text = "#000";
    color_header3_text = "#000";
    color_background_popup = "#C1CADE";
    frame_height = 46; };

  { style_name = "Light blue";
    color_background = "#B3E7FF"; 
    color_scrollbar_face = "#7CB1CA";  
    color_chunk2 = "#6BE4FF"; 
    color_scrollbar_highlight = "#E6F7FF"; 
    color_vd_page_background = "#9ED3EC";
    color_big_buttons_and_border_highlight = "#E6F7FF"; 
    color_input_text = "#9BDFFF"; 
    color_input_button = "#8CBFD7"; 
    color_chunk3 = "#7AF3FF"; 
    color_foreground_text_for_top_buttons = "#000";
    color_fbig_background = "#4EBCEF"; 
    color_bbig_background = "#9BCEE6"; 
    color_bsmall_back = "#A3D8F1"; 
    color_bsmall2 = "#91C4DC"; 
    color_bsmall3 = "#8CBFD7";
    color_border_of_top_buttons = "#5B8EA6"; 
    color_table_header_background = "#5CCBFF"; 
    color_mOver1_back = "#BFE5F7"; 
    color_mOver2_back = "#7FBCD9"; 
    color_mOver3_back = "#99D6F2";
    color_dl1_back = "#FFF"; 
    color_dl2_back = "#EEE"; 
    color_chunk0 = "#4DBCF0"; 
    color_chunk1 = "#48C1DC"; 
    color_vd_downloaded = "#63C3F0"; 
    color_vd_remaining = "#EEE";
    color_general_text = "#000"; 
    color_general_border = "#000"; 
    color_anchor = "#0000ff"; 
    color_anchor_hover = "#0000ff"; 
    color_download_anchor = "#000"; 
    color_download_anchor_hover = "#000";
    color_external_anchor = "#000"; 
    color_external_anchor_hover = "#000099"; 
    color_some_scrollbar = "#000"; 
    color_some_border = "#FFF"; 
    color_one_td_text = "#555";
    color_header2_text = "#000";
    color_header3_text = "#000";
    color_background_popup = "#C1CADE";
    frame_height = 46; };
  
  { style_name = "Light purple";
    color_background = "#CAB2E4"; 
    color_scrollbar_face = "#9982B3";  
    color_chunk2 = "#C29FE8"; 
    color_scrollbar_highlight = "#E1D7ED"; 
    color_vd_page_background = "#BEA5DA";
    color_big_buttons_and_border_highlight = "#E6E6E6"; 
    color_input_text = "#BE9EE3"; 
    color_input_button = "#A68FC0"; 
    color_chunk3 = "#D9B6FF"; 
    color_foreground_text_for_top_buttons = "#000";
    color_fbig_background = "#9360CD"; 
    color_bbig_background = "#B29DCC"; 
    color_bsmall_back = "#BDA5D7"; 
    color_bsmall2 = "#AB94C5"; 
    color_bsmall3 = "#A68FC0";
    color_border_of_top_buttons = "#786392"; 
    color_table_header_background = "#A06ED8"; 
    color_mOver1_back = "#BE9EE3"; 
    color_mOver2_back = "#D9C5F1"; 
    color_mOver3_back = "#C0A2E0";
    color_dl1_back = "#FFF"; 
    color_dl2_back = "#EEE"; 
    color_chunk0 = "#9A77C0"; 
    color_chunk1 = "#AE8BD4"; 
    color_vd_downloaded = "#9054D1"; 
    color_vd_remaining = "#EEE";
    color_general_text = "#000"; 
    color_general_border = "#000"; 
    color_anchor = "#0000ff"; 
    color_anchor_hover = "#0000ff"; 
    color_download_anchor = "#000"; 
    color_download_anchor_hover = "#000";
    color_external_anchor = "#000"; 
    color_external_anchor_hover = "#000099"; 
    color_some_scrollbar = "#000"; 
    color_some_border = "#FFF"; 
    color_one_td_text = "#555";
    color_header2_text = "#000";
    color_header3_text = "#000";
    color_background_popup = "#C1CADE";
    frame_height = 46; };
  
  { style_name = "Monochrome";
    color_background = "#C8C8C8"; 
    color_scrollbar_face = "#878787";  
    color_chunk2 = "#5D5D5D"; 
    color_scrollbar_highlight = "#E6E6E6"; 
    color_vd_page_background = "#A8A8A8";
    color_big_buttons_and_border_highlight = "#E6E6E6"; 
    color_input_text = "#B3B3B3"; 
    color_input_button = "#999"; 
    color_chunk3 = "#494949"; 
    color_foreground_text_for_top_buttons = "#000";
    color_fbig_background = "#686868"; 
    color_bbig_background = "#AAA"; 
    color_bsmall_back = "#B6B6B6"; 
    color_bsmall2 = "#9F9F9F"; 
    color_bsmall3 = "#999";
    color_border_of_top_buttons = "#5E5E5E"; 
    color_table_header_background = "#7F7F7F"; 
    color_mOver1_back = "#C1C1C1"; 
    color_mOver2_back = "#DFBDBD"; 
    color_mOver3_back = "#A4A4A4";
    color_dl1_back = "#FFF"; 
    color_dl2_back = "#EEE"; 
    color_chunk0 = "#989898"; 
    color_chunk1 = "#6C6C6C"; 
    color_vd_downloaded = "#424242"; 
    color_vd_remaining = "#EEE";
    color_general_text = "#000"; 
    color_general_border = "#000"; 
    color_anchor = "#0000ff"; 
    color_anchor_hover = "#0000ff"; 
    color_download_anchor = "#000"; 
    color_download_anchor_hover = "#000";
    color_external_anchor = "#000"; 
    color_external_anchor_hover = "#000099"; 
    color_some_scrollbar = "#000"; 
    color_some_border = "#FFF"; 
    color_one_td_text = "#555";
    color_header2_text = "#000";
    color_header3_text = "#000";
    color_background_popup = "#C1CADE";
    frame_height = 46; };

  { style_name = "Corona";
    color_background = "#C1CADE"; 
    color_scrollbar_face = "#8195D6";  
    color_chunk2 = "#BCCADC"; 
    color_scrollbar_highlight = "#FFF"; 
    color_vd_page_background = "#B7C0D4";
    color_big_buttons_and_border_highlight = "#FFF"; 
    color_input_text = "#FFF"; 
    color_input_button = "#6B80BF"; 
    color_chunk3 = "#B2C0D2"; 
    color_foreground_text_for_top_buttons = "#000";
    color_fbig_background = "#778BCC"; 
    color_bbig_background = "#95A9EA"; 
    color_bsmall_back = "#9AAEEF"; 
    color_bsmall2 = "#90A4E5"; 
    color_bsmall3 = "#869FE0";
    color_border_of_top_buttons = "#364A8B"; 
    color_table_header_background = "#687CBD"; 
    color_mOver1_back = "#6578BB"; 
    color_mOver2_back = "#CF82C5"; 
    color_mOver3_back = "#8195D6";
    color_dl1_back = "#FFF"; 
    color_dl2_back = "#EEE"; 
    color_chunk0 = "#5668AB"; 
    color_chunk1 = "#C1CFE1"; 
    color_vd_downloaded = "#6476B9"; 
    color_vd_remaining = "#EEE";
    color_general_text = "#000"; 
    color_general_border = "#000"; 
    color_anchor = "#0000ff"; 
    color_anchor_hover = "#0000ff"; 
    color_download_anchor = "#000"; 
    color_download_anchor_hover = "#000";
    color_external_anchor = "#000"; 
    color_external_anchor_hover = "#000099"; 
    color_some_scrollbar = "#000"; 
    color_some_border = "#FFF"; 
    color_one_td_text = "#555";
    color_header2_text = "#000";
    color_header3_text = "#000";
    color_background_popup = "#C1CADE";
    frame_height = 46; };

  { style_name = "Coronax";
    color_background = "#B1BACE"; 
    color_scrollbar_face = "#5165A6";  
    color_chunk2 = "#BCCADC"; 
    color_scrollbar_highlight = "#FFF"; 
    color_vd_page_background = "#B7C0D4";
    color_big_buttons_and_border_highlight = "#FFF"; 
    color_input_text = "#FFF"; 
    color_input_button = "#6B80BF"; 
    color_chunk3 = "#B2C0D2"; 
    color_foreground_text_for_top_buttons = "#000";
    color_fbig_background = "#778BCC"; 
    color_bbig_background = "#95A9EA"; 
    color_bsmall_back = "#9AAEEF"; 
    color_bsmall2 = "#90A4E5"; 
    color_bsmall3 = "#869FE0";
    color_border_of_top_buttons = "#364A8B"; 
    color_table_header_background = "#687CBD"; 
    color_mOver1_back = "#6578BB"; 
    color_mOver2_back = "#CF82C5"; 
    color_mOver3_back = "#5165A6";
    color_dl1_back = "#768FD0"; 
    color_dl2_back = "#E08686"; 
    color_chunk0 = "#5668AB"; 
    color_chunk1 = "#C1CFE1"; 
    color_vd_downloaded = "#6476B9"; 
    color_vd_remaining = "#EEE";
    color_general_text = "#D4C9B7"; 
    color_general_border = "#000"; 
    color_anchor = "#0000ff"; 
    color_anchor_hover = "#0000ff"; 
    color_download_anchor = "#000"; 
    color_download_anchor_hover = "#000";
    color_external_anchor = "#000"; 
    color_external_anchor_hover = "#000099"; 
    color_some_scrollbar = "#000"; 
    color_some_border = "#8195D6"; 
    color_one_td_text = "#555";
    color_header2_text = "#D4C9B7";
    color_header3_text = "#D4C9B7";
    color_background_popup = "#C1CADE";
    frame_height = 46; };
  
  { style_name = "Warm Colors Dark";
    color_background = "#31363B";
    color_scrollbar_face = "#616569";  
    color_scrollbar_highlight = "#616569"; 
    color_vd_page_background = "#616569";
    color_big_buttons_and_border_highlight = "#616569"; 
    color_input_text = "#e9b312"; 
    color_input_button = "#e9b312"; 
    color_foreground_text_for_top_buttons = "#ff0000";
    color_fbig_background = "#e9b311"; 
    color_bbig_background = "#232629"; 
    color_bsmall_back = "#e9b312"; 
    color_bsmall2 = "#e9b312"; 
    color_bsmall3 = "#232629";
    color_border_of_top_buttons = "#e9b312"; 
    color_table_header_background = "#232629"; 
    color_mOver1_back = "#e9b311"; 
    color_mOver2_back = "#e9b311"; 
    color_mOver3_back = "#e9b311";
    color_dl1_back = "#31363b"; 
    color_dl2_back = "#232629"; 
    color_chunk0 = "#FFFFFF"; 
    color_chunk1 = "#c1940e"; 
    color_chunk2 = "#624b07"; 
    color_chunk3 = "#151002"; 
    color_vd_downloaded = "#e9b311"; 
    color_vd_remaining = "#616569";
    color_general_text = "#FFFFFF"; 
    color_general_border = "#000"; 
    color_anchor = "#FFFFFF"; 
    color_anchor_hover = "#FFFFFF"; 
    color_download_anchor = "#e9b312"; 
    color_download_anchor_hover = "#e9b312";
    color_external_anchor = "#e9b312"; 
    color_external_anchor_hover = "#e9b312"; 
    color_some_scrollbar = "#e9b312"; 
    color_some_border = "#616569"; 
    color_one_td_text = "#e9b311";
    color_header2_text = "#ffffff";
    color_header3_text = "#ffffff";
    color_background_popup = "#e9b311";
    frame_height = 46; };

  { style_name = "Construction";
    color_background = "#C8C8C8"; 
    color_scrollbar_face = "#878787";  
    color_chunk2 = "#5D5D5D"; 
    color_scrollbar_highlight = "#E6E6E6"; 
    color_vd_page_background = "#A8A8A8";
    color_big_buttons_and_border_highlight = "#E6E6E6"; 
    color_input_text = "#B3B3B3"; 
    color_input_button = "#999"; 
    color_chunk3 = "#494949"; 
    color_foreground_text_for_top_buttons = "#000";
    color_fbig_background = "#686868"; 
    color_bbig_background = "#AAA"; 
    color_bsmall_back = "#B6B6B6"; 
    color_bsmall2 = "#9F9F9F"; 
    color_bsmall3 = "#999";
    color_border_of_top_buttons = "#5E5E5E"; 
    color_table_header_background = "#7F7F7F"; 
    color_mOver1_back = "#EAD040"; 
    color_mOver2_back = "#DFBDBD"; 
    color_mOver3_back = "#84A484";
    color_dl1_back = "#FFF"; 
    color_dl2_back = "#EEE"; 
    color_chunk0 = "#989898"; 
    color_chunk1 = "#6C6C6C"; 
    color_vd_downloaded = "#424242"; 
    color_vd_remaining = "#EEE";                    
    color_general_text = "#D4C9B7"; 
    color_general_border = "#000"; 
    color_anchor = "#0000ff"; 
    color_anchor_hover = "#0000ff"; 
    color_download_anchor = "#000"; 
    color_download_anchor_hover = "#000";
    color_external_anchor = "#000"; 
    color_external_anchor_hover = "#000099"; 
    color_some_scrollbar = "#000"; 
    color_some_border = "#8195D6"; 
    color_one_td_text = "#555";
    color_header2_text = "#D4C9B7";
    color_header3_text = "#D4C9B7";
    color_background_popup = "#C1CADE";
    frame_height = 46; } ]

let style_codes = [
  "@color_background@";
  "@color_scrollbar_face@";
  "@color_chunk2@";
  "@color_scrollbar_highlight@";
  "@color_vd_page_background@";
  "@color_big_buttons_and_border_highlight@";
  "@color_input_text@";
  "@color_input_button@";
  "@color_chunk3@";
  "@color_foreground_text_for_top_buttons@";
  "@color_fbig_background@";
  "@color_bbig_background@";
  "@color_bsmall_back@";
  "@color_bsmall2@";
  "@color_bsmall3@";
  "@color_border_of_top_buttons@";
  "@color_table_header_background@";
  "@color_mOver1_back@";
  "@color_mOver2_back@";
  "@color_mOver3_back@";
  "@color_dl1_back@";
  "@color_dl2_back@";
  "@color_chunk0@";
  "@color_chunk1@";
  "@color_vd_downloaded@";
  "@color_vd_remaining@";
  "@color_general_text@";
  "@color_general_border@";
  "@color_anchor@";
  "@color_anchor_hover@";
  "@color_download_anchor@";
  "@color_download_anchor_hover@";
  "@color_external_anchor@";
  "@color_external_anchor_hover@";
  "@color_some_scrollbar@";
  "@color_some_border@";
  "@color_one_td_text@";
  "@color_header2_text@";
  "@color_header3_text@";
  "@color_background_popup@";

(* legacy values *)
  "@C0@";"@C1@";"@C2@";"@C3@";
  "@C4@";"@C5@";"@C6@";"@C7@";
  "@C8@";"@C9@";"@C10@";"@C11@";
  "@C12@";"@C13@";"@C14@";"@C15@";
  "@C16@";"@C17@";"@C18@";"@C19@";
  "@C20@";"@C21@";"@C22@";"@C23@";
  "@C24@";"@C25@";"@C26@";"@C27@";
  "@C28@";"@C29@";"@C30@";"@C31@";
  "@C32@";"@C33@";"@C34@";"@C35@";
  "@C36@";"@C37@";"@C38@";"@C39@"]

(* code substitutions *)
let color_from_style stylenum code = 
  let style = styles.(stylenum) in
  match code with
  | "@color_background@" | "@C0@" -> style.color_background
  | "@color_scrollbar_face@" | "@C1@" -> style.color_scrollbar_face
  | "@color_chunk2@" | "@C2@" -> style.color_chunk2
  | "@color_scrollbar_highlight@" | "@C3@" -> style.color_scrollbar_highlight
  | "@color_vd_page_background@" | "@C4@" -> style.color_vd_page_background
  | "@color_big_buttons_and_border_highlight@" | "@C5@" -> style.color_big_buttons_and_border_highlight
  | "@color_input_text@" | "@C6@" -> style.color_input_text
  | "@color_input_button@" | "@C7@" -> style.color_input_button
  | "@color_chunk3@" | "@C8@" -> style.color_chunk3
  | "@color_foreground_text_for_top_buttons@" | "@C9@" -> style.color_foreground_text_for_top_buttons
  | "@color_fbig_background@" | "@C10@" -> style.color_fbig_background
  | "@color_bbig_background@" | "@C11@" -> style.color_bbig_background
  | "@color_bsmall_back@" | "@C12@" -> style.color_bsmall_back
  | "@color_bsmall2@" | "@C13@" -> style.color_bsmall2
  | "@color_bsmall3@" | "@C14@" -> style.color_bsmall3
  | "@color_border_of_top_buttons@" | "@C15@" -> style.color_border_of_top_buttons
  | "@color_table_header_background@" | "@C16@" -> style.color_table_header_background
  | "@color_mOver1_back@" | "@C17@" -> style.color_mOver1_back
  | "@color_mOver2_back@" | "@C18@" -> style.color_mOver2_back
  | "@color_mOver3_back@" | "@C19@" -> style.color_mOver3_back
  | "@color_dl1_back@" | "@C20@" -> style.color_dl1_back
  | "@color_dl2_back@" | "@C21@" -> style.color_dl2_back
  | "@color_chunk0@" | "@C22@" -> style.color_chunk0
  | "@color_chunk1@" | "@C23@" -> style.color_chunk1
  | "@color_vd_downloaded@" | "@C24@" -> style.color_vd_downloaded
  | "@color_vd_remaining@" | "@C25@" -> style.color_vd_remaining
  | "@color_general_text@" | "@C26@" -> style.color_general_text
  | "@color_general_border@" | "@C27@" -> style.color_general_border
  | "@color_anchor@" | "@C28@" -> style.color_anchor
  | "@color_anchor_hover@" | "@C29@" -> style.color_anchor_hover
  | "@color_download_anchor@" | "@C30@" -> style.color_download_anchor
  | "@color_download_anchor_hover@" | "@C31@" -> style.color_download_anchor_hover
  | "@color_external_anchor@" | "@C32@" -> style.color_external_anchor
  | "@color_external_anchor_hover@" | "@C33@" -> style.color_external_anchor_hover
  | "@color_some_scrollbar@" | "@C34@" -> style.color_some_scrollbar
  | "@color_some_border@" | "@C35@" -> style.color_some_border
  | "@color_one_td_text@" | "@C36@" -> style.color_one_td_text
  | "@color_header2_text@" | "@C37@" -> style.color_header2_text
  | "@color_header3_text@" | "@C38@" -> style.color_header3_text
  | "@color_background_popup@" | "@C39@" -> style.color_background_popup
  | _ -> assert false


let html_css_mods = ref ""
let download_html_css_mods = ref ""

let colour_changer () =
     html_css_mods := !!html_css_mods0;
     download_html_css_mods := !!download_html_css_mods0;
     let mstyle = ref !!CommonOptions.html_mods_style in
     if !mstyle >= Array.length styles || !mstyle < 0 then mstyle := 0;
     List.iter (fun c ->
       let color_regexp = Str.regexp c in
       html_css_mods := global_replace color_regexp
         (color_from_style !mstyle c) !html_css_mods;
       download_html_css_mods := global_replace color_regexp
         (color_from_style !mstyle c) !download_html_css_mods
     ) style_codes

let load_message_file () =
  (

(* Don't bother loading it for most users so their settings will always be current,
   without having to delete message_section for each new version.
   Users can set _load_message_section true if they want to modify and use their own.
   (reload_messages command)
*)
        if (not !!CommonOptions.html_mods) || (!!CommonOptions.html_mods && !!CommonOptions.html_mods_load_message_file) then begin
    try
      Options.load message_file
    with
      Sys_error _ ->
        (try Options.save message_file with _ -> ())
    | e ->
        lprintf_nl (_b "[cMe] Error %s loading message file %s")
          (Printexc2.to_string e)
        (Options.options_file_name message_file);
        lprintf_nl (_b "[cMe] Using default messages.");
  end
  )

