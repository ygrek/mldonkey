/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is the MLdonkey protocol handler 1.4.
 *
 * The Initial Developer of the Original Code is
 * Simon Peter <dn.tlp@gmx.net>.
 * Portions created by the Initial Developer are Copyright (C) 2003, 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Sven Koch
 * Len Walter <len@unsw.edu.au>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

/***** Defines *****/

// components defined in this file
const ED2KPROT_HANDLER_CONTRACTID =
    "@mozilla.org/network/protocol;1?name=ed2k";
const ED2KPROT_HANDLER_CID =
    Components.ID("{af8d664a-d002-438f-84a3-01f3a8ff325b}");

const MAGNETPROT_HANDLER_CONTRACTID =
    "@mozilla.org/network/protocol;1?name=magnet";
const MAGNETPROT_HANDLER_CID =
    Components.ID("{3e022170-58b0-4548-ba4c-1f47d54c7767}");

const SIG2DATPROT_HANDLER_CONTRACTID =
    "@mozilla.org/network/protocol;1?name=sig2dat";
const SIG2DATPROT_HANDLER_CID =
    Components.ID("{2a2e71ea-e857-4c71-9c93-04ff681df88a}");

// components used in this file
const NS_IOSERVICE_CID = "{9ac9e770-18bc-11d3-9337-00104ba0fd40}";
const NS_PREFSERVICE_CONTRACTID = "@mozilla.org/preferences-service;1";
const URI_CONTRACTID = "@mozilla.org/network/simple-uri;1";
const NS_WINDOWWATCHER_CONTRACTID = "@mozilla.org/embedcomp/window-watcher;1";
const STREAMIOCHANNEL_CONTRACTID = "@mozilla.org/network/stream-io-channel;1";

// interfaces used in this file
const nsIProtocolHandler    = Components.interfaces.nsIProtocolHandler;
const nsIURI                = Components.interfaces.nsIURI;
const nsISupports           = Components.interfaces.nsISupports;
const nsIIOService          = Components.interfaces.nsIIOService;
const nsIPrefService        = Components.interfaces.nsIPrefService;
const nsIWindowWatcher      = Components.interfaces.nsIWindowWatcher;
const nsIChannel            = Components.interfaces.nsIChannel;

// some misc. constants
const PREF_BRANCH   = "network.mldonkey.";
const WND_WIDTH = 320;
const WND_HEIGHT = 200;

// configuration (and defaults)
cfgUser   = "";
cfgPass   = "";
cfgServer = "localhost";
cfgPort   = "4080";

/***** MLdonkeyProtocolHandler *****/

function MLdonkeyProtocolHandler(scheme)
{
    this.scheme = scheme;
    this.readPreferences(PREF_BRANCH);
}

// attribute defaults
MLdonkeyProtocolHandler.prototype.defaultPort = -1;
MLdonkeyProtocolHandler.prototype.protocolFlags = nsIProtocolHandler.URI_NORELATIVE;

MLdonkeyProtocolHandler.prototype.allowPort = function(aPort, aScheme)
{
    return false;
}

MLdonkeyProtocolHandler.prototype.newURI = function(aSpec, aCharset, aBaseURI)
{
    var uri = Components.classes[URI_CONTRACTID].createInstance(nsIURI);
    uri.spec = aSpec;
    return uri;
}

MLdonkeyProtocolHandler.prototype.newChannel = function(aURI)
{
    // rewrite the URI into a http URL to the mldonkey server
    var myURI = "http://";
    if(cfgUser != "") myURI += cfgUser + ":" + cfgPass + "@";
    myURI += cfgServer + ":" + cfgPort + "/submit?q=dllink+" +
    encodeURIComponent(decodeURI(aURI.spec));

    // open up a window with our newly generated http URL
    var wwatch = Components.classes[NS_WINDOWWATCHER_CONTRACTID].getService(nsIWindowWatcher);
    var myWnd = wwatch.openWindow(null, myURI, "MLdonkey", null, null);

    // resize window to a reasonable size
    myWnd.outerWidth = WND_WIDTH;
    myWnd.outerHeight = WND_HEIGHT;

    // return a fake empty channel so current window doesn't change
    var chan = Components.classes[STREAMIOCHANNEL_CONTRACTID].createInstance(nsIChannel);
    return chan;
}

MLdonkeyProtocolHandler.prototype.readPreferences = function(pref_branch)
{
    // get preferences branch
    var PrefService = Components.classes[NS_PREFSERVICE_CONTRACTID].getService(nsIPrefService);
    var myPrefs = PrefService.getBranch(null);  // Mozilla bug #107617

    // read preferences (if available)
    if(myPrefs.getPrefType(pref_branch + "user") == myPrefs.PREF_STRING)
      cfgUser = myPrefs.getCharPref(pref_branch + "user");
    if(myPrefs.getPrefType(pref_branch + "pass") == myPrefs.PREF_STRING)
      cfgPass = myPrefs.getCharPref(pref_branch + "pass");
    if(myPrefs.getPrefType(pref_branch + "server") == myPrefs.PREF_STRING)
      cfgServer = myPrefs.getCharPref(pref_branch + "server");
    if(myPrefs.getPrefType(pref_branch + "port") == myPrefs.PREF_STRING)
      cfgPort = myPrefs.getCharPref(pref_branch + "port");
}

/***** MLdonkeyProtocolHandlerFactory *****/

function MLdonkeyProtocolHandlerFactory(scheme)
{
    this.scheme = scheme;
}

MLdonkeyProtocolHandlerFactory.prototype.createInstance = function(outer, iid)
{
    if(outer != null) throw Components.results.NS_ERROR_NO_AGGREGATION;

    if(!iid.equals(nsIProtocolHandler) && !iid.equals(nsISupports))
        throw Components.results.NS_ERROR_INVALID_ARG;

    return new MLdonkeyProtocolHandler(this.scheme);
}

var factory_ed2k = new MLdonkeyProtocolHandlerFactory("ed2k");
var factory_magnet = new MLdonkeyProtocolHandlerFactory("magnet");
var factory_sig2dat = new MLdonkeyProtocolHandlerFactory("sig2dat");

/***** Ed2kzillaModule *****/

var Ed2kzillaModule = new Object();

Ed2kzillaModule.registerSelf = function(compMgr, fileSpec, location, type)
{
    compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);

    // register ed2k protocol handler
    compMgr.registerFactoryLocation(ED2KPROT_HANDLER_CID,
                                    "ED2K protocol handler",
                                    ED2KPROT_HANDLER_CONTRACTID,
                                    fileSpec, location, type);

    // register magnet protocol handler
    compMgr.registerFactoryLocation(MAGNETPROT_HANDLER_CID,
                                    "Magnet protocol handler",
                                    MAGNETPROT_HANDLER_CONTRACTID,
                                    fileSpec, location, type);

    // register sig2dat protocol handler
    compMgr.registerFactoryLocation(SIG2DATPROT_HANDLER_CID,
                                    "Sig2dat protocol handler",
                                    SIG2DATPROT_HANDLER_CONTRACTID,
                                    fileSpec, location, type);
}

Ed2kzillaModule.unregisterSelf = function(compMgr, fileSpec, location)
{
    compMgr = compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);

    // unregister our components
    compMgr.unregisterFactoryLocation(ED2KPROT_HANDLER_CID, fileSpec);
    compMgr.unregisterFactoryLocation(MAGNETPROT_HANDLER_CID, fileSpec);
    compMgr.unregisterFactoryLocation(SIG2DATPROT_HANDLER_CID, fileSpec);
}

Ed2kzillaModule.getClassObject = function(compMgr, cid, iid)
{
    if(!iid.equals(Components.interfaces.nsIFactory))
        throw Components.results.NS_ERROR_NOT_IMPLEMENTED;

    if(cid.equals(ED2KPROT_HANDLER_CID)) return factory_ed2k;
    if(cid.equals(MAGNETPROT_HANDLER_CID)) return factory_magnet;
    if(cid.equals(SIG2DATPROT_HANDLER_CID)) return factory_sig2dat;

    throw Components.results.NS_ERROR_NO_INTERFACE;
}

Ed2kzillaModule.canUnload = function(compMgr)
{
    return true;    // our objects can be unloaded
}

/***** Entrypoint *****/

function NSGetModule(compMgr, fileSpec)
{
    return Ed2kzillaModule;
}
