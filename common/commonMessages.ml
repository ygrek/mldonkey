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
  
let message_file_name = try
    Sys.getenv "MLDONKEY_MESSAGES"
  with _ -> 
      Filename.concat CommonOptions.home_basedir ".mldonkey_messages.ini"

      (*
let _ =
  lprintf "Using Message File %s" message_file_name; lprint_newline ()
  *)

let message_file = Options.create_options_file message_file_name
let message name t x = define_option message_file [name] "" t x
let string name x = define_option message_file [name] "" string_option x

(* Please do not modify *_mods0, add/modify your own html_mods_style *)

let html_css_mods0 = define_option message_file ["html_css_mods0"] 
  "Main CSS style 0" 
    string_option  
"
pre {color: #000; font-family: Courier, Arial, Helvetica, sans-serif; font-size: 12px;}
p {color: #000; font-family: Verdana, Courier, Arial, Helvetica, sans-serif; font-size: 12px;}
body {
font-family: Verdana, sans-serif;
font-size: 12px;
scrollbar-face-color: #94AE94; scrollbar-shadow-color: #94AE94;
scrollbar-highlight-color: #E5FFE5; scrollbar-3dlight-color: #000000;
scrollbar-darkshadow-color: #000000; scrollbar-track-color: #CBE5CB;
scrollbar-arrow-color: #000000;
background: #CBE5CB; margin-top: 3px; margin-left: 5px; margin-right: 5px; }
a:link,a:active,a:visited { text-decoration: none; font-face: verdana; font-size: 10px; color: #000; }
a:hover { color: #000000; text-decoration: underline;}
.bu {
font-variant: small-caps; vertical-align: middle; white-space: nowrap;
background: #EAF4DE; color: #3D3D3D;
font-family: Verdana; font-size: 9px; line-height: 12px;
margin-top: 0px; margin-bottom: 0px;
padding-left: 6px; padding-right: 6px; padding-top: 1px; padding-bottom: 1px;
border-bottom: #FFF 0px solid; border-right: #FFF 0px solid; border-top: #92CA92 0px solid; border-left: #92CA92 0px solid;
}
.bbig {
text-align: center; font-size: 10px; font-family: Verdana; font-weight: 500;
border-top: #E5FFE5 1px solid; border-left: #E5FFE5 1px solid; border-bottom: #000 1px solid; border-right: #000 1px solid;
padding-left: 4px; padding-right: 4px; padding-top: 1px; padding-bottom: 1px;
color: #000; background: #B2CCB2;
}
.bsmall { background: #BCD6BC; }
.bsmall1 { background: #B2CCB2; }
.bsmall2 { background: #A8C2A8; }
.bsmall3 { background: #A3BDA3; }
.bbig2 {background: #A3BDA3; }
.bbig3 {background: #94AE94; }
.b1 { border-left: #718B71 solid 1px; border-top: #718B71 solid 1px; border-right: #718B71 solid 1px; border-bottom: #718B71 solid 1px; }
.b2 { border-left: #718B71 solid 0px; border-top: #718B71 solid 1px; border-right: #718B71 solid 1px; border-bottom: #718B71 solid 1px; }
.b3 { border-left: #718B71 solid 1px; border-top: #718B71 solid 0px; border-right: #718B71 solid 1px; border-bottom: #718B71 solid 1px; }
.b4 { border-left: #718B71 solid 0px; border-top: #718B71 solid 0px; border-right: #718B71 solid 1px; border-bottom: #718B71 solid 1px; }
.bb1 { border-left: #000 solid 1px; border-top: #E5FFE5 solid 1px; border-right: #E5FFE5 solid 1px; border-bottom: #000 solid 1px; }
.bb2 { border-left: #E5E5E5 solid 1px; border-top: #E5FFE5 solid 1px; border-right: #E5FFE5 solid 0px; border-bottom: #000 solid 1px; }
.bb3 { border-left: #E5E5E5 solid 1px; border-top: #E5FFE5 solid 1px; border-right: #000 solid 0px; border-bottom: #000 solid 0px; }
.bb4 { border-left: #E5E5E5 solid 1px; border-top: #E5FFE5 solid 1px; border-right: #000 solid 1px; border-bottom: #000 solid 0px; }
.src { border-left: #000 solid 0px; border-top: #000 solid 0px; border-right: #000 solid 1px; border-bottom: #000 solid 1px; }
.srctd { font-family: Verdana; font-size: 8px; }
td.fbig { cursor: pointer; padding-left: 2px; padding-right: 2px; font-family: Verdana; font-size: 10px; background: #86BE86; border-top: #000 solid 1px; border-left: #000 solid
1px; }
td.pr { border-right: #000 solid 1px; }
td, tr {font-size: 12px; font-face: verdana; }
td.sr { white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 4px; padding-right: 4px; font-family: verdana; font-size: 10px; color: #000;  }
td.srp { white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 0px; padding-right: 4px; font-family: verdana; font-size: 10px; color: #555;  }
td.srw { padding-top: 2px; padding-bottom: 2px; padding-left: 4px; padding-right: 4px; font-family: verdana; font-size: 10px; color: #000;  }
td.srh { cursor: pointer; vertical-align: top; background: #90C890; white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 4px; padding-right: 4px;
font-family: verdana; font-size: 10px; color: #000;  }
tr.dl-1, td.dl-1 { background: #FFFFFF; }
tr.dl-2, td.dl-2 { background: #EEEEEE; }
table.uploaders, table.friends, table.bw_stats, table.vo, table.cs, table.servers,
table.shares, table.downloaders, table.scan_temp, table.upstats, table.messages,
table.sources, table.shares, table.vc {
 margin-right: auto;
 margin-left: auto;
 border: 1;
 border: #000 solid 1px;
 border-collapse: collapse;
}
table.main { margin-right: auto; margin-left: auto; }
div.main, div.uploaders, div.friends, div.cs, div.shares, div.upstats, div.servers, div.vo,
div.downloaders, div.messages, div.vc, div.bw_stats, div.scan_temp { text-align: center; }
td.srb { padding-top: 1px; padding-bottom: 1px; font-size: 10px; font-family: Verdana; white-space: nowrap; border-right: #000 solid 1px; border-bottom: #000 solid 1px;
border-left: #000 solid 1px; border-top: #000 solid 0px; padding-left: 3px; padding-right: 3px;}
td.act { font-size: 10px; font-weight: 700; }
td.br {border-right: #000 dotted 1px;}
td.ar {text-align: right;}
td.al {text-align: left;}
td.ac {text-align: center;}
.chunk0 { background: #F33;}
.chunk1 { background: #33F;}
"

let html_js_mods0 = define_option message_file ["html_js_mods0"] 
  "Main JS include style 0" 
    string_option  
"
<!--
function mOvr(src,clrOver) {
 if (clrOver == undefined) {var clrOver='#94AE94'};
 src.style.cursor = 'pointer';
 src.style.backgroundColor = clrOver;
}
function mOut(src,clrIn) {
 src.style.cursor = 'default';
 src.style.backgroundColor = clrIn;
 src.bgColor = clrIn;
}
function mSub(target,cmd) {
 if (target != \"\") {
 	if (cmd==\"kill\") {
 		if (confirm(\"Are you sure?\")) {
     		top[target].location.href=\"/submit?q=\" + cmd;
 	    }
 	} else {
     top[target].location.href=\"/submit?q=\" + cmd;
 	}
 } else {
 location.href=\"/submit?q=\" + cmd;
 }               
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
top.fstatus.document.writeln(\"<html><head></head>\");
top.fstatus.document.writeln(\"<body bgColor='#CBE5CB'><center><table width=99% border=0 cellspacing=0 cellpadding=0>\");
top.fstatus.document.writeln(\" <form action=submit target=$O name=cmdFormular> \" );
top.fstatus.document.writeln(\"<tr><td width=100% nowrap>\");
top.fstatus.document.writeln(\" <input style='width: 99%; background: #E5FFE5; height: 20px; font-size: 12px;\'\"); 
top.fstatus.document.writeln(\" type=text name=q value=''> </td><td width=1>\");
top.fstatus.document.writeln(\"	<input style='border-bottom: #000 2px solid; border-right: #000 2px solid; border-top: #E5FFE5 2px solid; border-left: #E5FFE5 2px solid; color: #FFF; background: #A3BDA3; font-weight: 600; height: 20px; font-size: 10px;'\");
top.fstatus.document.writeln(\"type=submit value=Execute></td></form>\");
top.fstatus.document.writeln(\"</tr></table></body></html>\");
top.fstatus.document.close();
}
//-->
  "

let html_header_mods0 = define_option message_file ["html_header_mods0"] 
  "Header - style 0" 
    string_option
  "
<title>MLdonkey: Web Interface</title>
<link href=\"h.css\" rel=\"stylesheet\" type=\"text/css\">
<script language=\"javascript\" src=\"i.js\">
</script>
"

let download_html_css_mods0 = define_option message_file ["download_html_css_mods0"] "Download CSS - style 0" string_option  
"
body{background-color:#B2CCB2;color: #3F702E; font-family: Verdana, Arial, Helvetica, sans-serif;font-size: 13px; margin-top: 10px; margin: 2;}
td,pre {color: #3F702E; font-family: Verdana, Arial, Helvetica, sans-serif; font-size: 10px;}
table.downloaders { margin-right: auto; margin-left: auto; border: 1; border: #000 solid 1px;}
div.main { text-align: center; }
table.main { margin-right: auto; margin-left: auto; }
td.loaded{padding-top: 0px; padding-bottom: 0px; background-color:#72AA72; font-size:1px; line-height: 2px;}
td.remain{padding-top: 0px; padding-bottom: 0px; background-color:#EEE; font-size:1px; line-height: 2px;}
td.downloaded{font-family: Verdana; font-weight: 500; font-size: 12px; color: #000;}
td.dl { white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 5px; padding-right: 5px; font-family: verdana; font-size: 10px; color: #000;  }
td.dlheader {cursor: pointer; color: #000000; font-family: Verdana, serif; font-size: 10px;
border-bottom: solid 1px; background: #90C890; padding-left: 3px;
padding-right: 3px;}
input.checkbox { background: #90C890; vertical-align: middle; height: 10px; width: 10px; }
td.sr { white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 5px; padding-right: 5px; font-family: verdana; font-size: 10px; color: #000;  }
table { cellspacing: 0; cellpadding: 0; }
.commands { border: #000 solid 3px; }
td.ar {text-align: right;}
td.al {text-align: left;}
td.ac {text-align: center;}
td.big { border-top: #000 solid 1px; border-left: #000 solid 1px; }
td.pr { border-right: #000 solid 1px; }
.bigbutton { font-family: Verdana, serif; font-size: 10px; background: #CBE5CB; border: #CBE5CB solid 1px; cursor: pointer; }
.headbutton {
 width: 100%; font-family: Verdana, serif; font-size: 10px; border: #90C890 solid 1px; background: #90C890; 
 padding-left: 5px; padding-right: 5px; cursor: pointer;
}
tr.dl-1 { background: #FFFFFF; }
tr.dl-2 { background: #EEEEEE; }
input {font-family: tahoma; font-size: 10px}
a{ text-decoration: none; font-weight: bold;}
a:link,a:active,a:visited { color: #882222; }
a:hover { color: #000000; text-decoration: underline;}
a.extern:visited,a.extern:hover,a.extern:active { color: #000099; }
.extern:hover { color: #000000; }
"

let download_html_js_mods0 = define_option message_file ["download_html_js_mods0"] 
  "Download JS include style 0" 
    string_option  
"
<!--
function mOvr(src) {
 src.style.cursor = 'pointer';
 src.style.backgroundColor = '#B8DCB8';
}
function mOut(src,bg) {
 src.style.cursor = 'default';
 src.style.backgroundColor = bg;
 src.bgColor = bg;
}
//-->
"
  
let download_html_header_mods0 = define_option message_file ["download_html_header_mods0"] 
  "Download header - style 0" 
    string_option
  "
<title>MLDonkey: Web Interface</title>
<link href=\"dh.css\" rel=\"stylesheet\" type=\"text/css\">
<script language=\"javascript\" src=\"di.js\"></script>
  "
  
let web_common_header_mods0 = define_option message_file ["web_common_header_mods0"] 
  "Web header - style 0" 
    string_option 
"
<table width=100% border=0 cellspacing=0 cellpadding=0>
<tr><td>
<table style=\"border: #000 solid 1px; border-top: #E5FFE5 solid 1px; border-left: #E5FFE5 solid 1px\" class=\"topcommands\" cellspacing=0 cellpadding=0><tr>
<td
title=\"Help!\"
class=\"bu bsmall b1\"
onMouseOver=\"mOvr(this,'#FF0066');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','help')\">Help!
</td>
<td
title=\"MLDonkey homepage\"
class=\"bu bsmall1 b2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"top.output.location.href='http://www.mldonkey.net/'\">Homepage
</td>
<td
title=\"English/German support forums\"
class=\"bu bsmall1 b2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"top.output.location.href='http://www.mldonkeyworld.com/'\">Forums
</td>
<td
title=\"Options\"
class=\"bu bsmall2 b2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vo')\">Options
</td>
<td
title=\"Memory statistics\"
class=\"bu bsmall2 b2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','mem_stats')\">Mem
</td>
<td
title=\"Client statistics\"
class=\"bu bsmall2 b2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','client_stats')\">Client stats
</td>
<td
title=\"Load OverNet peers\"
class=\"bu bsmall3 b2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','ovweb')\">Load ONet peers
</td>
<td
title=\"Kill/Close the MLdonkey core\"
class=\"bu bsmall3 b2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','kill')\">Kill core
</td>
<tr>
<td
title=\"Show current version\"
class=\"bu bsmall b3\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','version')\">Version
</td>
<td 
title=\"View the CVS changeLog\"
class=\"bu bsmall1 b4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"top.output.location.href='http://savannah.nongnu.org/cgi-bin/viewcvs/*checkout*/mldonkey/mldonkey/distrib/ChangeLog?rev=HEAD&content-type=text/plain'\">ChangeLog
</td>
<td 
title=\"Friends\"
class=\"bu bsmall1 b4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','friends')\">Friends
</td>
<td 
title=\"Message window (20 second refresh)\"
class=\"bu bsmall2 b4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','message')\">Messages
</td>
<td 
title=\"View sources statistics\"
class=\"bu bsmall2 b4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','sources')\">Srcs
</td>
<td 
title=\"Client statistics in a table\"
class=\"bu bsmall2 b4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','cs')\">Stats table
</td>
<td 
title=\"Overnet statistics\"
class=\"bu bsmall3 b4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','ovstats')\">Overnet stats
</td>
<td 
title=\"Bandwidth stats (10 second refresh)\"
class=\"bu bsmall3 b4\" 
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','bw_stats')\">Bw stats
</td>
</tr>
</table>
<td></td>
<form action=\"submit\" target=\"$O\" name=\"cmdFormular\">
<td nowrap valign=top align=right>
    <input style=\"
    background: #E5FFE5;
    font-size: 10px;
    font-family: Verdana;
    width: 80px;
    \" type=\"text\" name=\"q\" size=13 value=\"\"><br><input style=\"
    color: #FFF;
    background: #A3BDA3;
    font-weight: 600;
    height: 18px;
    font-family: verdana;
    padding: 0px;
    font-size: 10px;
    height: 18px;
    width: 65px;
    \" type=\"submit\" value=\"Execute\"><input style=\"
    color: #FFF;
    background: #A3BDA3;
    font-weight: 600;
    height: 18px;
    font-family: verdana;
    padding: 0px;
    font-size: 10px;
    height: 18px;
    width: 15px;
    \" onclick=\"_cmdLine();\" type=\"button\" value=\"!\">
</td>
</form>

</tr>

<tr height=3><td colspan=3></td></tr>

<tr>
<td colspan=3>

<table style=\"border: #000 solid 1px\" class=\"commands\" width=100% cellspacing=0 cellpadding=0><tr>
<td
title=\"List connected servers\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vm')\">Connected servers
</td>
<td
title=\"Connect to more servers\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','c')\">Connect more servers
</td>
<td
title=\"Custom search\"
class=\"bu bbig bbig2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','view_custom_queries')\">Custom search
</td>
<td
title=\"View searches\"
class=\"bu bbig bbig2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vs')\">View searches
</td>
<td
title=\"Recover files from temp directory\"
class=\"bu bbig bbig3\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','recover_temp')\">Recover temp
</td>
<td
title=\"Commit downloaded files to incoming directory\"
class=\"bu bbig bbig3\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','commit')\">Commit</td>
<td
title=\"View current uploaders\"
class=\"bu bbig bbig3\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','uploaders')\">Ulers
</td>
<td
title=\"Upload statistics\"
class=\"bu bbig bbig3 bb2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','upstats')\">Uls
</td>
</tr>
<!-- Row -->
<tr>
<td
title=\"View the list of all servers\"
class=\"bu bbig bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vma')\">View all servers
</td>
<td
title=\"Remove old servers\"
class=\"bu bbig bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','remove_old_servers')\">Remove old Servers
</td>
<td
title=\"Extend search to more servers\"
class=\"bu bbig bbig2 bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','xs')\">Extend search
</td>
<td
title=\"View search results\"
class=\"bu bbig bbig2 bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vr')\">Search results
</td>
<td
title=\"Scan temp directory for files\"
class=\"bu bbig bbig3 bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','scan_temp')\">Scan temp
</td>
<td
title=\"Rescan for shared files\"
class=\"bu bbig bbig3 bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','reshare')\">Reshare
</td>
<td
title=\"View current downloaders\"
class=\"bu bbig bbig3 bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','downloaders')\">Dlers
</td>
<td
title=\"View current downloads\"
class=\"bu bbig bbig3 bb3\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vd')\">Dls
</td>
</tr>
</table>

</td>
</tr></table>
"

let html_css_mods1 = html_css_mods0

let html_js_mods1 = define_option message_file ["html_js_mods1"] 
  "Main JS include style 1" 
    string_option
"
<!--
function mOvr(src,clrOver) {
 if (clrOver == undefined) {var clrOver='#94AE94'};
 src.style.cursor = 'pointer';
 src.style.backgroundColor = clrOver;
}
function mOut(src,clrIn) {
 src.style.cursor = 'default';
 src.style.backgroundColor = clrIn;
 src.bgColor = clrIn;
}
function mSub(target,cmd) {
 if (target != \"\") {
 	if (cmd==\"kill\") {
 		if (confirm(\"Are you sure?\")) {
     		top[target].location.href=\"/submit?q=\" + cmd;
 	    }
 	} else {
     top[target].location.href=\"/submit?q=\" + cmd;
 	}
 } else {
 location.href=\"/submit?q=\" + cmd;
 }               
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
top.fstatus.document.writeln(\"<html><head></head>\");
top.fstatus.document.writeln(\"<body bgColor='#CBE5CB'><center><table width=99% border=0 cellspacing=0 cellpadding=0>\");
top.fstatus.document.writeln(\" <form action=submit target=$O name=cmdFormular> \" );
top.fstatus.document.writeln(\"<tr><td width=100% nowrap>\");
top.fstatus.document.writeln(\" <input style='width: 99%; background: #E5FFE5; height: 20px; font-size: 12px;\'\"); 
top.fstatus.document.writeln(\" type=text name=q value=''> </td><td width=1>\");
top.fstatus.document.writeln(\"	<input style='border-bottom: #000 2px solid; border-right: #000 2px solid; border-top: #E5FFE5 2px solid; border-left: #E5FFE5 2px solid; color: #FFF; background: #A3BDA3; font-weight: 600; height: 20px; font-size: 10px;'\");
top.fstatus.document.writeln(\"type=submit value=Execute></td></form>\");
top.fstatus.document.writeln(\"</tr></table></body></html>\");
top.fstatus.document.close();
}
function draw_middle_header() {
top.fstatus.document.open();top.fstatus.document.clear();
top.fstatus.document.writeln(\"<html><style type='text/css'>\");
top.fstatus.document.writeln(\"body{background-color:#CBE5CB;margin-top: 3px; margin-left: 5px; margin-right: 5px;} .bu {text-align: center;vertical-align: middle;white-space: nowrap;background: #B2CCB2;color: #000;font-family: Verdana;font-variant: small-caps; font-size: 10px;font-weight: 500;line-height: 12px;padding-left: 4px;padding-right: 4px;padding-top: 1px;padding-bottom: 1px;margin-top: 0px;margin-bottom: 0px; border-top: #E5FFE5 1px solid;border-left: #E5FFE5 1px solid;border-bottom: #000 1px solid;border-right: #000 1px solid;} .bb4 { border-left: #E5E5E5 solid 1px; border-top: #E5FFE5 solid 1px; border-right: #000 solid 1px; border-bottom: #000 solid 0px; } table.bw_stats {margin-right: auto;margin-left: auto;border: 1; border: #000 solid 1px; border-left: #000 solid 1px; border-right: #000 solid 0px}\");
top.fstatus.document.writeln(\"</style>\");
top.fstatus.document.write(\"<script language='javascript'><!\");
top.fstatus.document.writeln(\"--\");
top.fstatus.document.writeln(\"function mOvr(src,clrOver){src.style.cursor = 'pointer';src.style.backgroundColor = clrOver;}\");
top.fstatus.document.write(\"function mOut(src,clrIn){ src.style.cursor = 'default'; src.style.backgroundColor = clrIn; src.bgColor = clrIn;} //-\");
top.fstatus.document.writeln(\"-></script></head><body bgColor='#CBE5CB'><center><table cellspacing=0 cellpadding=0 class='bw_stats'><tr>\");
}
function close_page() {
top.fstatus.document.writeln(\"</tr></table></body></html>\");
top.fstatus.document.close(); }
function draw_td (tooltip,link,name,frame_name) {
if (frame_name==\"\") { frame_name='fstatus'};
top.fstatus.document.writeln(\"<td width=80 class='bu bb4' title='\" + tooltip + \"' onMouseOver=mOvr(this,'#BADEBA'); onMouseOut=mOut(this,this.bgColor); onClick=parent.\" + frame_name + \".location.href='\" + link + \"';>\" + name + \"</a></td>\"); }
function draw_scan_opts() { draw_middle_header();
draw_td('recover temp','/submit?q=recover_temp','recover temp','');close_page(); }
function draw_server_opts() {
draw_middle_header();
draw_td('view all servers','/submit?q=vma','view all','output');
draw_td('connect more','/submit?q=c','connect more','');
draw_td('remove old','/submit?q=remove_old_servers', 'remove old','');
close_page(); }
function draw_xs_search() { draw_middle_header ();
draw_td('extend search','/submit?q=xs','extend search','');close_page(); }
function draw_stats() { draw_middle_header();
draw_td('overnet statistics','/submit?q=ovstats','overnet','output');
draw_td('sources statistics','/submit?q=sources','sources','output');
draw_td('memory statistics','/submit?q=mem_stats','memory','output');
draw_td('old style statistics','/submit?q=client_stats','old style','output');
close_page(); }
//-->
"

let html_header_mods1 = define_option message_file ["html_header_mods1"] 
  "Header - style 1" 
    string_option 
  "
<title>MLDonkey: Web Interface</title>
<link href=\"h.css\" rel=\"stylesheet\" type=\"text/css\">
<script language=\"javascript\" src=\"i.js\"></script>
  "

let download_html_css_mods1 = download_html_css_mods0 

let download_html_js_mods1 = download_html_js_mods0

let download_html_header_mods1 = define_option message_file ["download_html_header_mods1"] "Download header - style 1" string_option 
  "
<title>MLdonkey: Web Interface</title>
<link href=\"dh.css\" rel=\"stylesheet\" type=\"text/css\">
<script language=\"javascript\" src=\"di.js\"></script>
"

let web_common_header_mods1 = define_option message_file ["web_common_header_mods1"] 
  "Web header - style 1" 
    string_option 
"
<table width=100% border=0 cellspacing=0 cellpadding=0>
<tr><td>
<table style=\"border: #000 solid 1px; border-top: #E5FFE5 solid 1px; border-left: #E5FFE5 solid 1px\" class=\"topcommands\" cellspacing=0 cellpadding=0><tr>
<td 
title=\"Upload statistics\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','upstats')\">Uploads
</td>
<td 
title=\"View current uploaders\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','uploaders')\">Ulers
</td>
<td 
title=\"List connected servers\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vm');draw_server_opts()\">Servers
</td>
<td 
title=\"Options\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vo')\">Options
</td>

<td 
title=\"Custom search\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','view_custom_queries')\">Search
</td>
<td 
title=\"Scan temp directory for files\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','scan_temp');draw_scan_opts()\">Show temp
</td>
<td 
title=\"Client statistics in a table\"
class=\"bu bbig\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','cs');draw_stats()\">Statistics
</td>

<td 
title=\"MLDonkey homepage\"
class=\"bu bbig bbig2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"top.output.location.href='http://www.mldonkey.net/'\">Homepage
</td>
<td 
title=\"English/German support forums\"
class=\"bu bbig bbig2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"top.output.location.href='http://www.mldonkeyworld.com/'\">Forums
</td>

<!--<td 
title=\"Load OverNet peers\"
class=\"bu bsmall bb2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','ovweb')\">ONet boot
</td>-->
<td 
title=\"Kill/Close the MLdonkey core\"
class=\"bu bbig bbig3 bb2\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','kill')\">Kill core
</td>
<tr> <!-- row -->
<td 
title=\"View current downloads\"
class=\"bu bbig bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vd');mSub('$S','bw_stats')\">Downloads
</td>
<td 
title=\"View current downloaders\"
class=\"bu bbig bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','downloaders')\">Dlers
</td>
<td 
title=\"Message window (20 second refresh)\"
class=\"bu bbig bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','message')\">Messages
</td>
<td 
title=\"Friends\"
class=\"bu bbig bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','friends')\">Friends
</td>
<td 
title=\"View searches\"
class=\"bu bbig bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','vs');draw_xs_search()\">View searches <!-- added -->
</td>
<td 
title=\"Rescan for shared files\"
class=\"bu bbig bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','reshare')\">Reshare
</td>
<td 
title=\"Bandwidth stats (10 second refresh)\"
class=\"bu bbig bb4 \"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','bw_stats')\">Bw stats
</td>
<td 
title=\"View the CVS changeLog\"
class=\"bu bbig bbig2 bb4\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"top.output.location.href='http://savannah.nongnu.org/cgi-bin/viewcvs/*checkout*/mldonkey/mldonkey/distrib/ChangeLog?rev=HEAD&content-type=text/plain'\">ChangeLog
</td>
<td 
title=\"Help!\"
class=\"bu bbig bbig2 bb4\"
onMouseOver=\"mOvr(this,'#FF0066');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$O','help')\">Help!
</td>
<td 
title=\"Show current version\"
class=\"bu bbig bbig3 bb3\"
onMouseOver=\"mOvr(this,'#BADEBA');\"
onMouseOut=\"mOut(this,this.bgColor);\"
onClick=\"mSub('$S','version')\">Version
</td>

</tr>
</table>
<td></td>
<form action=\"submit\" target=\"$O\" name=\"cmdFormular\">
<td nowrap valign=top align=right>
	<input style=\"
	background: #E5FFE5;
	font-size: 10px;
	font-family: Verdana;
	width: 80px;
	\" type=\"text\" name=\"q\" size=13 value=\"\"><br><input style=\"	
	color: #FFF;
	background: #A3BDA3;
	font-weight: 600;
	height: 18px;
	font-family: verdana;
	padding: 0px;
	font-size: 10px;
	height: 18px;
	width: 65px;
	\" type=\"submit\" value=\"Execute\"><input style=\"	
	color: #FFF;
	background: #A3BDA3;
	font-weight: 600;
	height: 18px;
	font-family: verdana;
	padding: 0px;
	font-size: 10px;
	height: 18px;
	width: 15px;
	\" onclick=\"_cmdLine();\" type=\"button\" value=\"!\">
</td>
</form>
</tr></table>
"

let html_css_old = define_option message_file
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

let html_js_old = define_option message_file
  ["html_js_old"]
  "The old js"
    string_option  
  "
<!--
function CheckInput(){
var cmdString = document.cmdFormular.q.value;
if (cmdString.substr(0,7) == \"ed2k://\"){
var cmdValue = \"dllink \" + document.cmdFormular.q.value;
document.cmdFormular.q.value = cmdValue;

}else if (cmdString.substr(0,6) == \"fha://\"){
var cmdValue = \"ovlink \" + document.cmdFormular.q.value;
document.cmdFormular.q.value = cmdValue;
}
return true; 
}
//-->
  "

let html_header_old = define_option message_file ["html_header_old"]
  "The header used in the WEB interface (modify to add your CSS)"
    string_option  
  "<title>MLDonkey: Web Interface</title>
<link href=\"h.css\" rel=\"stylesheet\" type=\"text/css\">
<script language=\"javascript\" src=\"i.js\"></script>
    "
  
let download_html_css_old = define_option message_file ["download_html_css_old"]
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
  
let download_html_js_old = define_option message_file ["download_html_js_old"]
  "The old js"
    string_option  
"
<!--
function ovlink(){
var cmdValue = \"ovlink \" + document.cmdFormular.q.value;
document.cmdFormular.q.value = cmdValue;
document.cmdFormular.submit();
}


function ed2k(){
var cmdValue = \"dllink \" + document.cmdFormular.q.value;
document.cmdFormular.q.value = cmdValue;
document.cmdFormular.submit(); 
}
//-->
  "
  
let download_html_header_old = define_option message_file ["download_html_header_old"]
  "The header used in the WEB interface for downloads (modify to add your CSS)"
    string_option  
  "
<title>MLdonkey: Web Interface</title>
<link href=\"dh.css\" rel=\"stylesheet\" type=\"text/css\">
<script language=\"javascript\" src=\"di.js\"></script>
"

    
let web_common_header_old = define_option message_file
    ["web_common_header_old"] "The header displayed in the WEB interface"
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
<td><a href=\"/files\" onMouseOver=\"window.status='View current downloads status';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Downloads</a></td>
<td><a href=\"/submit?q=view_custom_queries\" onMouseOver=\"window.status='Send a customized query';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Custom search</a></td>
<td><a href=\"/submit?q=vm\" onMouseOver=\"window.status='View current connection status';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Connected servers</a></td>
<td><a href=\"/submit?q=help\" onMouseOver=\"window.status='View a list of all available commands';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Help</a></td>
<td><a href=\"/submit?q=vo\" onMouseOver=\"window.status='View and change your preferences';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Preferences</a></td>
</tr>
<tr>
<td><a href=\"/submit?q=upstats\" onMouseOver=\"window.status='View current upload status';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Uploads</a></td>
<td><a href=\"/submit?q=xs\" onMouseOver=\"window.status='Extend your search to more servers';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Extend search</a></td>
<td><a href=\"/submit?q=c\" onMouseOver=\"window.status='Connect to more servers';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Connect more servers</a></td>
<td><a href=\"/submit?q=version\" onMouseOver=\"window.status='Check version of MLDonkey';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Version</a></td>
<td><a href=\"/submit?q=remove_old_servers\" onMouseOver=\"window.status='Remove servers that have not been connected for several days';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Clean old servers</a></td>
</tr>
<tr>
<td><a href=\"/submit?q=uploaders\" onMouseOver=\"window.status='View a list of your upload slots';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Upload slots</a></td>
<td><a href=\"/submit?q=vs\" onMouseOver=\"window.status='View a list of your sent queries';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>View searches</a></td>
<td><a href=\"/submit?q=vma\" onMouseOver=\"window.status='View a list of all known servers';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>View all servers</a></td>
<td><a href=\"/submit?q=client_stats\" onMouseOver=\"window.status='Gives stats about your transfers';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Client stats</a></td>
<td><a href=\"/submit?q=reshare\" onMouseOver=\"window.status='Check shared files for removal';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Reshare files</a></td>
<td><a href=\"/submit?q=html_mods\" onMouseOver=\"window.status='Toggle html_mods';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Toggle html_mods</a></td>
</tr>
<tr>
<td><a href=\"/submit?q=commit\" onMouseOver=\"window.status='Move finished downloads to incoming directory';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Commit</a></td>
<td><a href=\"/submit?q=vr\" onMouseOver=\"window.status='View results to your queries';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Search results</a></td>
<td><a href=\"/submit?q=ovweb\" onMouseOver=\"window.status='Boot Overnet peers from http list';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $S>Load Overnet peers</a></td>
<td><a class=\"extern\" href=\"http://www.mldonkeyworld.com/\" onMouseOver=\"window.status='MLDonkey World';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>English forum</a></td>
<td><a class=\"extern\" href=\"http://www.mldonkey.org/\" onMouseOver=\"window.status='German Forum';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>German forum</a></td>
<td><a href=\"/submit?q=kill\" onMouseOver=\"window.status='Save and quit MLDonkey';return true;\" onMouseOut=\"window.status='';return true;\" onFocus=\"this.blur();\" $O>Kill MLDonkey</a></td>
  </tr>
  </table>
"

  
let available_commands_are = string "available_commands_are" 
  "Available commands are:\n"
  
let command_not_authorized = string "command_not_authorized"
  "Command not authorized\n use 'auth <password>' before."

let bad_login = string "bad_login"
  "Bad login/password"
  
let full_access = string "full_access"
  "Full access enabled"

let download_started = message "download_started"
    (T.boption (T.int T.bformat)) "Download of file %d started<br>"

let no_such_command = message "no_such_command"
    (T.boption (T.string T.bformat))   "No such command %s\n"
  
let bad_number_of_args = string "bad_number_of_args" 
    "Bad number of arguments"
  
let _ =
  (
    try
      Options.load message_file
    with
      Sys_error _ ->
        (try Options.save message_file with _ -> ())
    | e ->
        lprintf "Error %s loading message file %s"
          (Printexc2.to_string e) 
        (Options.options_file_name message_file);
        lprint_newline ();
        lprintf "Using default messages."; lprint_newline ();
  )
