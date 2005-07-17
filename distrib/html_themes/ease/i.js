
<!--
var mOvrClass='';
function mOvr(src,clrOver) {
 if (clrOver == undefined) {var clrOver='mOvr3'};
 mOvrClass = src.className;
 src.className = mOvrClass + ' ' + clrOver;
}
function mOut(src) {
 src.className=mOvrClass;
}
function mSub(target,cmd) {
 if (target != "") {
 	if (cmd=="kill") {
 		if (confirm("Are you sure?")) {
     		top[target].location.href="submit?q=" + cmd;
 	    }
 	} else {
     top[target].location.href="submit?q=" + cmd;
 	}
 } else {
 location.href="submit?q=" + cmd;
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
			_raw = _raw.replace((new RegExp('\\\(','gi')), '');
		   if (_raw.indexOf(":") != -1) { _raw = _raw.substring(2,99); }
		 if (_raw.search(new RegExp("[TGMk]","i"))) {
		  if (_raw.indexOf("T") != -1) { _raw = parseFloat(_raw) * 1024 * 1024 * 1024 * 1024; }
		  else {
			if (_raw.indexOf("G") != -1) { _raw = parseFloat(_raw) * 1024 * 1024 * 1024; }
			else {
				 if (_raw.indexOf("M") != -1) { _raw = parseFloat(_raw) * 1024 * 1024; }
				 else {
					if (_raw.indexOf("k") != -1) { _raw = parseFloat(_raw) * 1024; }
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
		if (a.s=="") { if (b.s !="") { return 1;} }
		if (b.s=="") { if (a.s !="") { return -1;} }
	}
	if (a.s.toUpperCase() < b.s.toUpperCase()) {return -1;}
	if (a.s.toUpperCase() > b.s.toUpperCase()) {return 1;}
	return 0;
}
function _cmpFloat(a,b) {
	if (!_tabMode) {
		if (a.s=="") { if (b.s !="") { return -1;} }
		if (b.s=="") { if (a.s !="") { return 1;} }
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
top.fstatus.document.writeln("<html><head>");
top.fstatus.document.writeln("<link href='h.css' rel='stylesheet' type='text/css'>");
top.fstatus.document.writeln("</head><body><center><table width=99% border=0 cellspacing=0 cellpadding=0>");
top.fstatus.document.writeln("<form action=submit target=output name=cmdFormular> " );
top.fstatus.document.writeln("<tr><td width=100% nowrap>");
top.fstatus.document.writeln(" <input class='txt' style='width: 99%; height: 20px; font-size: 12px;'");
top.fstatus.document.writeln(" type=text name=q value=''> </td><td width=1>");
top.fstatus.document.writeln("	<input class='but' style='color: #FFF; font-weight: 600; height: 20px; font-size: 10px;'");
top.fstatus.document.writeln("type=submit value=Execute></td></form>");
top.fstatus.document.writeln("</tr></table></body></html>");
top.fstatus.document.close();
}
function draw_middle_header() {
top.fstatus.document.open();top.fstatus.document.clear();
top.fstatus.document.writeln("<html><head>");
top.fstatus.document.writeln("<link href='h.css' rel='stylesheet' type='text/css'>");
top.fstatus.document.writeln('<script language="javascript" src="i.js">');
top.fstatus.document.writeln("</script>");
top.fstatus.document.writeln("</head><body><center><table cellspacing=0 cellpadding=0 class='bw_stats'><tr>");
}
function close_page() {
top.fstatus.document.writeln("</tr></table></body></html>");
top.fstatus.document.close(); }

function draw_td (tooltip,link,command,frame_name) {
if (frame_name=="") { frame_name='fstatus'} else frame_name='output';
top.fstatus.document.writeln("<td width=80 class='bu bbig' title='" + tooltip + "' onMouseOver=mOvr(this); onMouseOut=mOut(this); onClick=parent." + frame_name + ".location.href='" + "submit?q=" + link + "';>" + command + "</a></td>"); }

function draw_scan_opts() { draw_middle_header();
draw_td('recover temp','recover_temp','recover temp','');close_page(); }

function draw_server_opts() {
draw_middle_header();
draw_td('view all servers','vma','view all','o');
draw_td('connect more servers','c','connect more','');
draw_td('remove old servers','remove_old_servers', 'remove old','');
draw_td('remove all servers','rem%20all', 'remove all','');
close_page(); }

function draw_xs_search() { draw_middle_header();
draw_td('extend search','xs','extend search','');close_page(); }

function draw_stats() { draw_middle_header();
draw_td('overnet statistics','ovstats','overnet','o');
draw_td('sources statistics','sources','sources','o');
draw_td('memory statistics','mem_stats','memory','o');
draw_td('old style statistics','client_stats','old style','o');
close_page(); }

function draw_options() {draw_middle_header();
draw_td('client','voo+1','client','o');
draw_td('ports','voo+2','ports','o');
draw_td('display','voo+3','display','o');
draw_td('delays','voo+4','delays','o');
draw_td('files','voo+5','files','o');
draw_td('mail','voo+6','mail','o');
draw_td('net','voo+7','net','o');
draw_td('all','voo','all','o');
close_page(); }

if (document.layers) {navigator.family = "nn4"}
if (document.all) {navigator.family = "ie4"}
if (window.navigator.userAgent.toLowerCase().match("gecko")) {navigator.family = "gecko"}

overdiv="0";
function popLayer(a){
if (navigator.family == "gecko") {pad="0"; bord="1 bordercolor=black";}
else {pad="1"; bord="0";}
desc = "<table cellspacing=0 cellpadding="+pad+" border="+bord+"  bgcolor=000000><tr><td>\n"
	+"<table cellspacing=0 cellpadding=10 border=0 width=100%><tr><td bgcolor=#DDFFFF><center><font size=-1>\n"
	+a
	+"\n</td></tr></table>\n"
	+"</td></tr></table>";
if(navigator.family =="nn4") {
	document.object1.document.write(desc);
	document.object1.document.close();
	document.object1.left=x+15;
	document.object1.top=y-5;
	}
else if(navigator.family =="ie4"){
	object1.innerHTML=desc;
	object1.style.pixelLeft=x+15;
	object1.style.pixelTop=y-5;
	}
else if(navigator.family =="gecko"){
	document.getElementById("object1").innerHTML=desc;
	document.getElementById("object1").style.left=x+15;
	document.getElementById("object1").style.top=y-5;
	}
}

function hideLayer(){
if (overdiv == "0") {
	if(navigator.family =="nn4") {eval(document.object1.top="-500");}
	else if(navigator.family =="ie4"){object1.innerHTML="";}
	else if(navigator.family =="gecko") {document.getElementById("object1").style.top="-500";}
	}
}

var isNav = (navigator.appName.indexOf("Netscape") !=-1);
function handlerMM(e){

// oder Plazierung neben Mauszeiger
x = (isNav) ? e.pageX : event.clientX + document.body.scrollLeft;
y = (isNav) ? e.pageY : event.clientY + document.body.scrollTop;

}
if (isNav){document.captureEvents(Event.MOUSEMOVE);}
document.onmousemove = handlerMM;

//-->
