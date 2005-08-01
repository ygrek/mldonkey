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
      Filename.concat CommonOptions.config_dir "messages.ini"


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
background: @C0@; margin-top: 3px; margin-left: 5px; margin-right: 5px;
font-family: Verdana, sans-serif; font-size: 12px;
scrollbar-face-color: @C1@; scrollbar-shadow-color: @C1@;
scrollbar-highlight-color: @C3@; scrollbar-3dlight-color: @C34@;
scrollbar-darkshadow-color: @C34@; scrollbar-track-color: @C0@;
scrollbar-arrow-color: @C34@; }
table.commands { border: @C27@ solid 1px; background: @C0@ }
table.topcommands { background: @C0@; border: @C27@ solid 1px; border-top: @C3@ solid 1px; border-left: @C3@ solid 1px; }
pre { color: @C26@; font-family: Courier, Arial, Helvetica, sans-serif; font-size: 12px; }
p { color: @C26@; font-family: Verdana, Courier, Arial, Helvetica, sans-serif; font-size: 12px; }
input.txt { background: @C6@ }
input.txt2 { background: @C11@;
font: 12px courier; padding: 0px;
width: 38px; height: 18px; line-height: 14px; color: @C26@;
border-right: @C35@ 2px solid; border-top: @C27@ 1px solid; border-left: @C27@ 1px solid; border-bottom: @C35@ 2px solid; }
input.but2 { background: @C14@;
border: 0px; padding: 0px; font: bold 10px verdana;
width: 36px; height: 14px; }
input.but { background: @C7@ };

a:link,a:active,a:visited { text-decoration: none; font-face: verdana;
font-size: 10px; color: @C28@; }
a:hover { color: @C29@; text-decoration: underline;}
.bu {
vertical-align: middle; white-space: nowrap;
background: @C8@; color: @C9@;
font-family: Verdana; font-size: 9px; line-height: 12px;
margin-top: 0px; margin-bottom: 0px;
padding-left: 6px; padding-right: 6px; padding-top: 1px; padding-bottom: 1px;
border: @C35@ 0px solid; }
.bbig {
text-align: center; font-size: 10px; font-family: Verdana; font-weight: 500;
border-top: @C3@ 1px solid; border-left: @C3@ 1px solid; border-bottom: @C27@ 1px solid; border-right: @C27@ 1px solid;
padding-left: 4px; padding-right: 4px; padding-top: 1px; padding-bottom: 1px;
color: @C26@; background: @C11@; }
.bbigm {
text-align: center; font: bold 10px verdana;
border-top: @C3@ 1px solid; border-left: @C3@ 1px solid; border-bottom: @C27@ 1px solid; border-right: @C27@ 1px solid;
padding-left: 4px; padding-right: 4px; padding-top: 1px; padding-bottom: 1px;
color: @C26@; background: @C14@; }
.bsmall { background: @C12@; }
.bsmall1 { background: @C11@; }
.bsmall2 { background: @C13@; }
.bsmall3 { background: @C14@; }
.bbig2 { background: @C14@; }
.bbig3 { background: @C1@; }
.b1 { border-left: @C15@ solid 1px; border-top: @C15@ solid 1px; border-right: @C15@ solid 1px; border-bottom: @C15@ solid 1px; }
.b2 { border-left: @C15@ solid 0px; border-top: @C15@ solid 1px; border-right: @C15@ solid 1px; border-bottom: @C15@ solid 1px; }
.b3 { border-left: @C15@ solid 1px; border-top: @C15@ solid 0px; border-right: @C15@ solid 1px; border-bottom: @C15@ solid 1px; }
.b4 { border-left: @C15@ solid 0px; border-top: @C15@ solid 0px; border-right: @C15@ solid 1px; border-bottom: @C15@ solid 1px; }
.bb1 { border-left: @C27@ solid 1px; border-top: @C3@ solid 1px; border-right: @C3@ solid 1px; border-bottom: @C27@ solid 1px; }
.bb2 { border-left: @C5@ solid 1px; border-top: @C3@ solid 1px; border-right: @C3@ solid 0px; border-bottom: @C27@ solid 1px; }
.bb3 { border-left: @C5@ solid 1px; border-top: @C3@ solid 1px; border-right: @C27@ solid 0px; border-bottom: @C27@ solid 0px; }
.bb4 { border-left: @C5@ solid 1px; border-top: @C3@ solid 1px; border-right: @C27@ solid 1px; border-bottom: @C27@ solid 0px; }
.src { border-left: @C27@ solid 0px; border-top: @C27@ solid 0px; border-right: @C27@ solid 1px; border-bottom: @C27@ solid 1px; }
.srctd { font-family: Verdana; font-size: 8px; }
td.fbig { color: @C26@; cursor: pointer; padding-left: 2px; padding-right: 2px; font-family: Verdana; font-size: 10px; background: @C10@;
border-top: @C27@ solid 1px; border-left: @C27@ solid 1px; }
td.pr { border-right: @C27@ solid 1px; }
td.fbigb { border-top: @C27@ solid 0px; border-bottom: @C27@ solid 1px; }
td, tr {font-size: 12px; font-face: verdana; }
td.sr { white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 4px; padding-right: 4px; font-family: verdana; font-size: 10px; color: @C26@; }
td.srp { white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 0px; padding-right: 4px; font-family: verdana; font-size: 10px; color: @C36@; }
td.srw { padding-top: 2px; padding-bottom: 2px; padding-left: 4px; padding-right: 4px; font-family: verdana; font-size: 10px; color: @C26@; }
td.srh { cursor: pointer; vertical-align: top; background: @C16@; white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 4px; padding-right: 4px;
font-family: verdana; font-size: 10px; color: @C26@;  }
tr.dl-1, td.dl-1 { background: @C20@; }
tr.dl-2, td.dl-2 { background: @C21@; }
.mOvr1, tr.mOvr1 {background: @C17@; cursor: pointer; }
.mOvr2, tr.mOvr2 {background: @C18@; cursor: pointer; }
.mOvr3, tr.mOvr3 {background: @C19@; cursor: pointer; }
table.uploaders, table.friends, table.bw_stats, table.vo, table.cs, table.servers,
table.shares, table.downloaders, table.scan_temp, table.upstats, table.messages,
table.shares, table.vc, table.results, table.networkInfo {
 margin-right: auto;
 margin-left: auto;
 border: 1;
 border: @C27@ solid 1px;
 border-collapse: collapse; }
table.sourcesInfo, table.serversC { width: 100%; margin-right: auto; margin-left: auto; border: 1; border: @C27@ solid 1px; border-collapse: collapse; }
table.sources {border: 1; border: @C27@ solid 1px; border-collapse: collapse; }
table.main { margin-right: auto; margin-left: auto; }
div.main, div.uploaders, div.friends, div.cs, div.shares, div.upstats, div.servers, div.serversC, div.vo,
div.downloaders, div.messages, div.vc, div.bw_stats, div.scan_temp, div.results { }
td.srb { padding-top: 1px; padding-bottom: 1px; font-size: 10px; font-family: Verdana; white-space: nowrap; border-right: @C27@ solid 1px; border-bottom: @C27@ solid 1px;
border-left: @C27@ solid 1px; border-top: @C27@ solid 0px; padding-left: 3px; padding-right: 3px; }
td.act { font-size: 10px; font-weight: 700; }
td.br {border-right: @C27@ dotted 1px;}
td.ar {text-align: right;}
td.al {text-align: left;}
td.ac {text-align: center;}
.chunk0 { left:0px; top:0px; height:12px; background: @C22@}
.chunk1 { left:0px; top:0px; height:12px; background: @C23@}
.chunk2 { left:0px; top:0px; height:12px; background: @C2@}
.chunk3 { left:0px; top:0px; height:12px; background: @C8@}
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
function mSub(target,cmd) {
 if (target != \"\") {
 	if (cmd==\"kill\") {
 		if (confirm(\"Are you sure?\")) {
     		top[target].location.href=\"submit?q=\" + cmd;
 	    }
 	} else {
        if (cmd.substring(0,6)==\"custom\") {top[target].location.href=\"submit?\" + cmd;}
        else {top[target].location.href=\"submit?q=\" + cmd;
     }
 	}
 } else {
 location.href=\"submit?q=\" + cmd;
 }
}
function showTab(t){
	for (i=1; i<=6; i++) document.getElementById(\"tab\" + i).style.display = \"none\";
	document.getElementById(\"tab\" + t).style.display = \"block\";
}
var _tabLast=null;
function _rObj (s,ar) {
 this.s = s;
 this.ar = ar;
}
function _tabCreateArray(obj,st){
	var tb=obj.parentNode.parentNode;
	var rw=obj.parentNode.parentNode.rows;
	var _nRows=rw.length;
	var _tabS=new Array(_nRows-1);
	var _nCells = rw.item(0).cells.length;
	for(var i=1;i<_nRows;i++){
		var _raw = rw.item(i).cells.item(obj.cellIndex).innerHTML;
		if (st==1) {
			_raw = _raw.replace((new RegExp('\\\\\\(','gi')), '');
		   if (_raw.indexOf(\":\") != -1) { _raw = _raw.substring(2,99); }
		 if (_raw.search(new RegExp(\"[TGMk]\",\"i\"))) {
		  if (_raw.indexOf(\"T\") != -1) { _raw = parseFloat(_raw) * 1024 * 1024 * 1024 * 1024; }
		  else {
			if (_raw.indexOf(\"G\") != -1) { _raw = parseFloat(_raw) * 1024 * 1024 * 1024; }
			else {
				 if (_raw.indexOf(\"M\") != -1) { _raw = parseFloat(_raw) * 1024 * 1024; }
				 else {
					if (_raw.indexOf(\"k\") != -1) { _raw = parseFloat(_raw) * 1024; }
				 }
			}
	      }
		}}
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
function _tabSort(obj,st){
	if (_tabLast==obj) {_tabMode=!(_tabMode);}
	else {_tabMode=true;}
	_tabCreateArray(obj,st);
	_tabLast=obj;
	return _tabMode;
}
function _cmdLine(){
top.fstatus.document.open();
top.fstatus.document.clear();
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

function dllink() {
	var l = prompt( \"enter ed2k, sig2dat, torrent or other link\", \"\" );
	if( l != null ) {
		var f = document.forms[\"cmdFormular\"];
		var t = f.elements[\"q\"].value;
		f.elements[\"q\"].value = \"dllink \" + l;
		f.submit();
		f.elements[\"q\"].value = t;
	}
}

//-->
  "


let html_header_mods0 = define_option message_section ["html_header_mods0"]
  "Header - style 0"
    string_option
  "
<title>MLdonkey: Web Interface</title>
<meta name=\"generator\" content=\"MLDonkey\" />
<meta name=\"robots\" content=\"noindex,nofollow\" />
<link rel=\"shortcut icon\" href=\"favicon.ico\" type=\"image/x-icon\" />
<link href=\"h.css\" rel=\"stylesheet\" type=\"text/css\" />
<script language=\"javascript\" src=\"i.js\">
</script>
"

let download_html_css_mods0 = define_option message_section ["download_html_css_mods0"]
  "Download CSS - style 0"
    string_option
"
body{ background-color:@C4@;color: @C26@; font-family: Verdana, Arial, Helvetica, sans-serif;font-size: 13px; margin-top: 10px; margin: 2;}
td,pre { color: @C26@; font-family: Verdana, Arial, Helvetica, sans-serif; font-size: 10px; }
table.downloaders { margin-right: auto; margin-left: auto; border: 1; border: @C27@ solid 1px; }
div.main {  }
table.main { margin-right: auto; margin-left: auto; }
td.loaded{ padding-top: 0px; padding-bottom: 0px; background-color:@C24@; font-size:1px; line-height: 2px; }
td.remain{ padding-top: 0px; padding-bottom: 0px; background-color:@C25@; font-size:1px; line-height: 2px; }
td.downloaded{ font-family: Verdana; font-weight: 500; font-size: 12px; color: @C26@; }
td.dl { white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 5px; padding-right: 5px; font-family: verdana; font-size: 10px; color: @C26@;  }
td.dlheader { cursor: pointer; color: @C26@; font-family: Verdana, serif; font-size: 10px;
border-bottom: solid 1px; background: @C16@; padding-left: 3px;
padding-right: 3px; }
input.checkbox { background: @C16@; vertical-align: middle; height: 10px; width: 10px; }
td.sr { white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 5px; padding-right: 5px; font-family: verdana; font-size: 10px; color: @C26@;  }
table { cellspacing: 0; cellpadding: 0; }
td.ar { text-align: right; }
td.al { text-align: left; }
td.ac { text-align: center; }
td.brs { border-right: @C27@ solid 1px; padding-left: 2px; padding-right: 2px; text-align: center; }
td.np { padding-left: 2px; padding-right: 0px; text-align: center; }
td.big { border-top: @C27@ solid 1px; border-left: @C27@ solid 1px; }
td.pr { border-right: @C27@ solid 1px; }
.bigbutton { color: @C26@; font-family: Verdana, serif; font-size: 10px; background: @C0@; border: @C0@ solid 1px; cursor: pointer; }
.headbutton {
 width: 100%; font-family: Verdana, serif; font-size: 10px; border: @C16@ solid 1px; background: @C16@;
 padding-left: 5px; padding-right: 5px; cursor: pointer; }
tr.dl-1 { background: @C20@; }
tr.dl-2 { background: @C21@; }
tr.mOvrDL, .mOvrDL { background: @C17@; cursor: pointer; }
input { font-family: tahoma; font-size: 10px; }
a{ text-decoration: none; font-weight: bold;}
a:link,a:active,a:visited { color: @C30@; }
a:hover { color: @C31@; text-decoration: underline; }
a.extern:visited,a.extern:hover,a.extern:active { color: @C32@; }
.extern:hover { color: @C33@; }
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
  "
<title>MLDonkey: Web Interface</title>
<link href=\"dh.css\" rel=\"stylesheet\" type=\"text/css\">
<script language=\"javascript\" src=\"di.js\"></script>
  "

let web_common_header_mods0 = define_option message_section ["web_common_header_mods0"]
  "Web header - style 0"
    string_option
"
<!-- Main Table -->
<TABLE BORDER=0 cellspacing=1 cellpadding=0 width=\"100%\"><TR>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\"Transfers Tab\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(1);mSub('fstatus','bw_stats');mSub('output','vd');\">Transfers</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\"Searches Tab\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(2);mSub('fstatus','view_custom_queries');mSub('output','custom=Complex+Search');\">Search</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\"Servers Tab\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(3);mSub('output','vm');\">Servers</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\"Statistics Tab\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(4);mSub('output','stats');\">Statistics</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\"Options Tab\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(5);mSub('output','voo+1');\">Options</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\"Help+Miscellaneous Tab\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"showTab(6);mSub('fstatus','version');mSub('output','help');\">Help+</TD></TR></TBODY></TABLE></TD>
<TD width=85><TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR><TD class=\"bu bbigm\" title=\"dllink\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onclick=\"dllink();\">DL</TD></TR></TBODY></TABLE></TD>
<FORM name=cmdFormular action=submit target=output>
<TD noWrap width=100% title=\\\"Input mldonkey commands here\\\"><TABLE cellSpacing=0 cellpadding=0 width=\"100%\"><TBODY><TR>
<TD style=\"padding: 0px; border: 0px; padding-left: 5px;\" title=\"Input mldonkey command here\">
<INPUT class=\"txt2\" style=\"WIDTH: 99%;\" name=q>
</TD></TR></TBODY></TABLE></TD><TD noWrap>
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\"><TBODY><TR>
<TD class=\"bu bbigm\" style=\"padding-top: 0px; padding-bottom: 0px;\" title=\"Input Command\">
<INPUT class=\"but2\" type=submit value=\"Input\">
</TD></TR></TBODY></TABLE></TD></FORM></TR></TABLE>
<!-- End Main Table -->

<DIV ID=\"tab1\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\"Current downloads\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','vd')\">Downloads</TD>
<TD class=\"bu bbig\" title=\"Current downloaders\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','downloaders')\">Downloaders</TD>
<TD class=\"bu bbig\" title=\"Upload statistics\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','upstats')\">Uploads</TD>
<TD class=\"bu bbig\" title=\"Uploaders\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','uploaders')\">Uploaders</TD>
<TD class=\"bu bbig\" title=\"Commit downloaded files to incoming directory\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','commit')\">Commit</TD>
<TD class=\"bu bbig\" title=\"Check shared files for removal\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','reshare')\">Reshare</TD>
<TD class=\"bu bbig\" title=\"List contents of the temp directory\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','scan_temp')\">Scan temp</TD>
<TD class=\"bu bbig\" title=\"Bandwidth statistics (set html_mods_bw_refresh_delay)\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','bw_stats')\">Bandwidth stats</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab2\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\"Extend search to more servers and view results\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','xs');mSub('output','vr');\">Extend search</TD>
<TD class=\"bu bbig\" title=\"View search results\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','vr')\">Search results</TD>
<TD class=\"bu bbig\" title=\"View searches\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','vs')\">View searches</TD>
<TD class=\"bu bbig\" title=\"Complex search\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','custom=Complex+Search')\">Complex search</TD>
<TD class=\"bu bbig\" title=\"MP3 search\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','custom=MP3+Search')\">MP3 search</TD>
<TD class=\"bu bbig\" title=\"Movie search\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','custom=Movie+Search')\">Movie search</TD>
<TD class=\"bu bbig\" title=\"Album search\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','custom=Album+Search')\">Album search</TD>
<TD class=\"bu bbig\" title=\"Force download (click after trying to download the duplicate file)\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','force_download')\">Force DL</TD>
<TD class=\"bu bbig\" title=\"View RSS feeds\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','rss')\">RSS</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab3\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\"List connected servers\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','vm')\">Connected servers</TD>
<TD class=\"bu bbig\" title=\"List all servers\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','vma')\">All servers</TD>
<TD class=\"bu bbig\" title=\"Connect to more servers\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','c')\">Connect to more servers</TD>
<TD class=\"bu bbig\" title=\"Remove old servers\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','remove_old_servers')\">Remove old servers</TD>
<TD class=\"bu bbig\" title=\"Open Serverlist\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"top.output.location.href='http://ed2k.2x4u.de/list.html'\">Serverlist</TD>
</TR></TBODY></TABLE></DIV>
<DIV ID=\"tab4\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\"eDonkey statistics in a table\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','cs')\">eDonkey Table</TD>
<TD class=\"bu bbig\" title=\"eMule MODs statistics\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','csm')\">eMule MODs</TD>
<TD class=\"bu bbig\" title=\"Overnet statistics\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"showTab(4);mSub('fstatus','ov_view_stats_cmds');mSub('output','ov_stats')\">Overnet</TD>
<TD class=\"bu bbig\" title=\"Kademlia statistics\" 
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"showTab(4);mSub('fstatus','kad_view_stats_cmds');mSub('output','kad_stats')\">Kademlia</TD>
<TD class=\"bu bbig\" title=\"Gnutella statistics\" 
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','gstats')\">Gnutella</TD>
<TD class=\"bu bbig\" title=\"Gnutella2 statistics\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','g2stats')\">Gnutella2</TD>
<TD class=\"bu bbig\" title=\"Memory statistics\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','mem_stats 0')\">Memory</TD>
<TD class=\"bu bbig\" title=\"Sources statistics\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','sources')\">Sources</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab5\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\"Settings\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','voo+1')\">Settings</TD>
<TD class=\"bu bbig\" title=\"View/edit shared directories\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','shares')\">Shares</TD>
<TD class=\"bu bbig\" title=\"Friends\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','friends')\">Friends</TD>
<TD class=\"bu bbig\" title=\"View/send messages (20 second refresh)\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','message')\">Messages</TD>
<TD class=\"bu bbig\" title=\"IP blocking statistics\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','block_list')\">IP blocking</TD>
<TD class=\"bu bbig\" title=\"Recover files from temp directory\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','recover_temp');mSub('output','scan_temp');\">Recover temp</TD>
<TD class=\"bu bbig\" title=\"Close all files (use to free space on disk after remove)\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('fstatus','close_fds')\">Close files</TD>
<TD class=\"bu bbig\" title=\"View all clients\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','vc+all')\">View clients</TD>
<TD class=\"bu bbig\" title=\"View web infos\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','vwi')\">View web infos</TD>
</TR></TBODY></TABLE></DIV>

<DIV ID=\"tab6\" style=\"display: none\">
<TABLE class=commands cellSpacing=0 cellPadding=0 width=\"100%\">
<TBODY><TR>
<TD class=\"bu bbig\" title=\"Long help\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','longhelp')\">LongHelp</TD>
<TD class=\"bu bbig\" title=\"Network listing\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','networks')\">Networks</TD>
<TD class=\"bu bbig\" title=\"Buildinfo\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','buildinfo')\">Buildinfo</TD>
<TD class=\"bu bbig\" title=\"View ChangeLog\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"top.output.location.href='http://savannah.nongnu.org/cgi-bin/viewcvs/mldonkey/mldonkey/distrib/ChangeLog?rev=HEAD&content-type=text/vnd.viewcvs-markup'\">ChangeLog</TD>
<TD class=\"bu bbig\" title=\"HomePage\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"top.output.location.href='http://www.mldonkey.net/'\">Homepage</TD>
<TD class=\"bu bbig\" title=\"Wiki (User updated FAQ/documentation)\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"top.output.location.href='http://mldonkey.berlios.de/modules.php?name=Wiki'\">Wiki</TD>
<TD class=\"bu bbig\" title=\"Support forums (english/german)\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"top.output.location.href='http://www.mldonkeyworld.com/'\">Support forums</TD>
<TD class=\"bu bbig\" title=\"View core log\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','log')\">Log</TD>
<TD class=\"bu bbig\" title=\"Kill core\"
onMouseOver=\"mOvr(this,'mOvr1');\" onMouseOut=\"mOut(this);\"
onClick=\"mSub('output','kill')\">Kill core</TD>
</TR></TBODY></TABLE></DIV>
"

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
<script language=\"javascript\" src=\"i.js\"></script>
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
<script language=\"javascript\" src=\"di.js\"></script>
"

let web_common_header_old = define_option message_section ["web_common_header_old"]
  "The header displayed in the WEB interface"
    string_option
  "
<table width=\"100%\" border=\"0\">
<tr>
<td align=\"left\" valign=\"middle\" width=\"*\"><a href=\"http://www.mldonkey.net/\" $O><b>MLDonkey Home</b></a></td>
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
<td><a class=\"extern\" href=\"http://www.mldonkeyworld.com/\" onMouseOver=\"window.status='MLDonkey World';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>English forum</a></td>
<td><a class=\"extern\" href=\"http://www.mldonkey.org/\" onMouseOver=\"window.status='German Forum';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>German forum</a></td>
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

let bad_number_of_args = _s    "Bad number of arguments"

(* Colour arrays list for debug and devs

Main CSS :

body { background: @C0@;    
scrollbar-face-color: @C1@; scrollbar-shadow-color: @C1@;
scrollbar-highlight-color: @C3@; scrollbar-3dlight-color: @C34@;
scrollbar-darkshadow-color: @C34@; scrollbar-track-color: @C0@;
scrollbar-arrow-color: @C34@; }
table.commands { border: @C27@; background: @C0@;}
table.topcommands { background: @C0@; border: @C27@; border-top: @C3@; border-left: @C3@;} 
pre { color: @C26@; }
p { color: @C26@; }
input.txt { background: @C6@; }
input.txt2 { background: @C11@; color: @C26@;
border-right: @C35@; border-top: @C27@; border-left: @C27@; border-bottom: @C35@; }
input.but2 { background: @C14@; }
input.but { background: @C7@; }
a:link,a:active,a:visited { color: @C28@; }
a:hover { color: @C29@; }
.bu { background: @C8@; color: @C9@; border: @C35@; }
.bbig { border-top: @C3@;  border-left: @C3@;  border-bottom: @C27@;  border-right: @C27@;
color: @C26@; background: @C11@; }
.bbigm { border-top: @C3@; border-left: @C3@; border-bottom: @C27@; border-right: @C27@;
color: @C26@; background: @C14@; }
.bsmall { background: @C12@; }
.bsmall1 { background: @C11@; }
.bsmall2 { background: @C13@; }
.bsmall3 { background: @C14@; }
.bbig2 { background: @C14@; }
.bbig3 { background: @C1@; }
.b1 { border-left: @C15@; border-top: @C15@; border-right: @C15@; border-bottom: @C15@; }
.b2 { border-left: @C15@; border-top: @C15@; border-right: @C15@; border-bottom: @C15@; }
.b3 { border-left: @C15@; border-top: @C15@; border-right: @C15@; border-bottom: @C15@; }
.b4 { border-left: @C15@; border-top: @C15@; border-right: @C15@; border-bottom: @C15@; }
.bb1 { border-left: @C27@; border-top: @C3@; border-right: @C3@; border-bottom: @C27@; }
.bb2 { border-left: @C5@; border-top: @C3@; border-right: @C3@; border-bottom: @C27@; }
.bb3 { border-left: @C5@; border-top: @C3@; border-right: @C27@; border-bottom: @C27@; }
.bb4 { border-left: @C5@; border-top: @C3@; border-right: @C27@; border-bottom: @C27@; }
.src { border-left: @C27@; border-top: @C27@; border-right: @C27@; border-bottom: @C27@; }
td.fbig { background: @C10@; border-top: @C27@; border-left: @C27@; }
td.pr { border-right: @C27@; }
td.fbigb { border-top: @C27@; border-bottom: @C27@; }
td.sr { color: @C26@; }
td.srp { color: @C36@; }
td.srw { color: @C26@; }
td.srh { vertical-align: top; background: @C16@; color: @C26@; }
tr.dl-1, td.dl-1 { background: @C20@; }
tr.dl-2, td.dl-2 { background: @C21@; }
.mOvr1, tr.mOvr1 {background: @C17@; }
.mOvr2, tr.mOvr2 {background: @C18@; }
.mOvr3, tr.mOvr3 {background: @C19@; }
table.uploaders, table.friends, table.bw_stats, table.vo, table.cs, table.servers,
table.shares, table.downloaders, table.scan_temp, table.upstats, table.messages,
table.shares, table.vc, table.results, table.networkInfo { border: @C27@; }
table.sourcesInfo, table.serversC { border: @C27@; }
table.sources { border: @C27@;}
td.srb { border-right: @C27@; border-bottom: @C27@;
border-left: @C27@; border-top: @C27@; }
td.br { border-right: @C27@;}
.chunk0;{ background: @C22@}
.chunk1 { background: @C23@}
.chunk2 { background: @C2@}
.chunk3 { background: @C8@}

Download CSS
body{ background-color:@C4@; color: @C26@; }
td,pre { color: @C26@; }
table.downloaders { border: @C27@;}
td.loaded{ background-color:@C24@; }
td.remain{ background-color:@C25@; }
td.downloaded{ color: @C26@;}
td.dl { color: @C26@;  }
td.dlheader { color: @C26@;  
border-bottom: ( background: @C16@; }
input.checkbox { background: @C16@; }
td.sr { color: @C26@; }
td.brs { border-right: @C27@; }
td.big { border-top: @C27@;  border-left: @C27@; }
td.pr { border-right: @C27@; }
.bigbutton { background: @C0@; border: @C0@; color: @C26@; }
.headbutton { border: @C16@; background: @C16@; }
tr.dl-1 { background: @C20@; }
tr.dl-2 { background: @C21@; }
tr.mOvrDL, .mOvrDL { background: @C17@;  }
a:link,a:active,a:visited { color: @C30@; }
a:hover { color: @C31@; }
a.extern:visited,a.extern:hover,a.extern:active { color: @C32@; }
.extern:hover { color: @C33@; }

0 - background                       13 - bsmall2 - (options, memstats)
1 - scrollbar face                   14 - bsmall3 - (load onet peers)
2 - chunk2                           15 - border color of top buttons
3 - scrollbar highlight color        16 - table header background
4 - vd page background               17 - mOver1 back
5 - big buttons, border highlight    18 - mOver2 back
6 - input.txt                        19 - mOver3 back
7 - input.but                        20 - dl-1 back
8 - chunk3                           21 - dl-2 back
9 - foreground text for top buttons  22 - chunk0
10 - fbig background (tabs)          23 - chunk1
11 - bbig background (vma button)    24 - vd downloaded
12 - bsmall back (Help!)             25 - vd remaining

26 - general text
27 - general border
28 - anchor
29 - anchor hover
30 - download anchor
31 - download hover anchor
32 - external anchor
33 - external hover anchor
34 - some scrollbar
35 - some border
36 - one td text
*)

let ncarr = ref 8
let carr = Array.create !ncarr [||]
let _ = (
    (* Default green *)
    carr.(0) <- [| "#CBE5CB"; "#94AE94";  "#33F"; "#E5FFE5"; "#B2CCB2";
                    "#E5E5E5"; "#BADEBA"; "#A3BDA3"; "#00F"; "#3D3D3D";
                    "#86BE86"; "#B2CCB2"; "#BCD6BC"; "#A8C2A8"; "#A3BDA3";
                    "#718B71"; "#90C890"; "#BADEBA"; "#F00"; "#94AE94";
                    "#FFF"; "#EEE"; "#F33"; "#1010DC" ; "#72AA72"; "#EEE";
                    "#000"; "#000"; "#000"; "#000"; "#000"; "#000";
                    "#000"; "#000099"; "#000"; "FFF"; "#555" |];
	(* Orange Tang *)
    carr.(1) <- [| "#EEE"; "FF8800";  "FF8800"; "#FAD08F"; "F5F5F5";
                    "#F2C074"; "#FF9900"; "#FF9900"; "#FF7700"; "#FFF";
                    "#FF9D2D"; "#FF8800"; "#FF8800"; "#FF8800"; "#FF8800";
                    "#FF9900"; "#FF8800"; "#FACF8E"; "#FACF8E"; "#FACF8E";
                    "#FFF"; "#EEE"; "#DDD"; "#FACD88" ; "#FF9900"; "#EEE";
                    "#000"; "#000"; "#000"; "#000"; "#000"; "#000";
                    "#000"; "#000099"; "#000"; "FFF"; "#555" |];
    (* Light blue *)
    carr.(2) <- [| "#B3E7FF"; "#7CB1CA";  "#6BE4FF"; "#E6F7FF"; "#9ED3EC";
                    "#E6F7FF"; "#9BDFFF"; "#8CBFD7"; "#7AF3FF"; "#000";
                    "#4EBCEF"; "#9BCEE6"; "#A3D8F1"; "#91C4DC"; "#8CBFD7";
                    "#5B8EA6"; "#5CCBFF"; "#BFE5F7"; "#7FBCD9"; "#99D6F2";
                    "#FFF"; "#EEE"; "#4DBCF0"; "#48C1DC"; "#63C3F0"; "#EEE";
                    "#000"; "#000"; "#000"; "#000"; "#000"; "#000";
                    "#000"; "#000099"; "#000"; "FFF"; "#555" |];
    (* Light purple *)
    carr.(3) <- [| "#CAB2E4"; "#9982B3";  "#C29FE8"; "#E1D7ED"; "#BEA5DA";
                    "#E6E6E6"; "#BE9EE3"; "#A68FC0"; "#D9B6FF"; "#000";
                    "#9360CD"; "#B29DCC"; "#BDA5D7"; "#AB94C5"; "#A68FC0";
                    "#786392"; "#A06ED8"; "#BE9EE3"; "#D9C5F1"; "#C0A2E0";
                    "#FFF"; "#EEE"; "#9A77C0"; "#AE8BD4"; "#9054D1"; "#EEE";
                    "#000"; "#000"; "#000"; "#000"; "#000"; "#000";
                    "#000"; "#000099"; "#000"; "FFF"; "#555" |];
    (* Monochrome *)
    carr.(4) <- [| "#C8C8C8"; "#878787";  "#5D5D5D"; "#E6E6E6"; "#A8A8A8";
                    "#E6E6E6"; "#B3B3B3"; "#999"; "#494949"; "#000";
                    "#686868"; "#AAA"; "#B6B6B6"; "#9F9F9F"; "#999";
                    "#5E5E5E"; "#7F7F7F"; "#C1C1C1"; "#DFBDBD"; "#A4A4A4";
                    "#FFF"; "#EEE"; "#989898"; "#6C6C6C"; "#424242"; "#EEE";
                    "#000"; "#000"; "#000"; "#000"; "#000"; "#000";
                    "#000"; "#000099"; "#000"; "FFF"; "#555" |];
    (* Corona *)
    carr.(5) <- [| "#C1CADE"; "#8195D6";  "#BCCADC"; "#FFF"; "#B7C0D4";
                    "#FFF"; "#FFF"; "#6B80BF"; "#B2C0D2"; "#000";
                    "#778BCC"; "#95A9EA"; "#9AAEEF"; "#90A4E5"; "#869FE0";
                    "#364A8B"; "#687CBD"; "#6578BB"; "#CF82C5"; "#8195D6";
                    "#FFF"; "#EEE"; "#5668AB"; "#C1CFE1"; "#6476B9"; "#EEE";
                    "#000"; "#000"; "#000"; "#000"; "#000"; "#000";
                    "#000"; "#000099"; "#000"; "FFF"; "#555" |];
    (* Coronax *)
    carr.(6) <- [| "#B1BACE"; "#5165A6";  "#BCCADC"; "#FFF"; "#B7C0D4";
                    "#FFF"; "#FFF"; "#6B80BF"; "#B2C0D2"; "#000";
                    "#778BCC"; "#95A9EA"; "#9AAEEF"; "#90A4E5"; "#869FE0";
                    "#364A8B"; "#687CBD"; "#6578BB"; "#CF82C5"; "#5165A6";
                    "#768FD0"; "#E08686"; "#5668AB"; "#C1CFE1"; "#6476B9"; "#EEE";
                    "#D4C9B7"; "#000"; "#000"; "#000"; "#000"; "#000";
                    "#000"; "#000099"; "#000"; "#8195D6"; "#555" |];
    (* Construction *)
    carr.(7) <- [| "#C8C8C8"; "#878787";  "#5D5D5D"; "#E6E6E6"; "#A8A8A8";
                    "#E6E6E6"; "#B3B3B3"; "#999"; "#494949"; "#000";
                    "#686868"; "#AAA"; "#B6B6B6"; "#9F9F9F"; "#999";
                    "#5E5E5E"; "#7F7F7F"; "#ead040"; "#DFBDBD"; "#84A484";
                    "#FFF"; "#EEE"; "#989898"; "#6C6C6C"; "#424242"; "#EEE" |];
)

let html_css_mods = ref ""
let download_html_css_mods = ref ""

let colour_changer () =
     html_css_mods := !!html_css_mods0;
     download_html_css_mods := !!download_html_css_mods0;
	let mstyle = ref !!CommonOptions.html_mods_style in
	if (!mstyle > !ncarr || !mstyle < 0) then mstyle := 0;
     Array.iteri (fun i c ->
      html_css_mods := global_replace (Str.regexp (Printf.sprintf "@C%d@" i))
       carr.(!mstyle).(i) !html_css_mods;
      download_html_css_mods := global_replace (Str.regexp (Printf.sprintf "@C%d@" i))
       carr.(!mstyle).(i) !download_html_css_mods
     ) carr.(!mstyle)

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
