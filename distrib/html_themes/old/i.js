
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
 if (target != "") {
 	if (cmd=="kill") {
 		if (confirm("Are you sure?")) {
     		top[target].location.href="submit?q=" + cmd;
 	    }
 	} else {
        if (cmd.substring(0,6)=="custom") {top[target].location.href="submit?" + cmd;}
        else {top[target].location.href="submit?q=" + cmd;
     }
 	}
 } else {
 location.href="submit?q=" + cmd;
 }               
}
function showTab(t){
	for (i=1; i<=6; i++) document.getElementById("tab" + i).style.display = "none";
	document.getElementById("tab" + t).style.display = "block";
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
//-->
  