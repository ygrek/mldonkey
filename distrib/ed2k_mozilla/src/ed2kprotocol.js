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
 * The Original Code is the MLdonkey ed2k protocol handler.
 *
 * The Initial Developer of the Original Code is
 * Simon Peter <dn.tlp@gmx.net>.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
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

// components used in this file
const STANDARDURL_CONTRACTID = "@mozilla.org/network/standard-url;1";
const NS_IOSERVICE_CID = "{9ac9e770-18bc-11d3-9337-00104ba0fd40}";
const NS_PREFSERVICE_CONTRACTID = "@mozilla.org/preferences-service;1";

// interfaces used in this file
const nsIProtocolHandler    = Components.interfaces.nsIProtocolHandler;
const nsIURI                = Components.interfaces.nsIURI;
const nsIStandardURL        = Components.interfaces.nsIStandardURL;
const nsISupports           = Components.interfaces.nsISupports;
const nsIIOService          = Components.interfaces.nsIIOService;
const nsIPrefService        = Components.interfaces.nsIPrefService;

// some misc. constants
const PREF_BRANCH   = "network.ed2k."

// configuration (and defaults)
cfgUser   = "admin";
cfgPass   = "";
cfgServer = "localhost";
cfgPort   = "4080";

/***** ED2KProtocolHandler *****/

// exported attributes
ED2KProtocolHandler.prototype.scheme = "ed2k";
ED2KProtocolHandler.prototype.defaultPort = -1;
ED2KProtocolHandler.prototype.protocolFlags = nsIProtocolHandler.URI_NORELATIVE;

function ED2KProtocolHandler()
{
}

ED2KProtocolHandler.prototype.allowPort = function(aPort, aScheme)
{
    return false;
}

ED2KProtocolHandler.prototype.newURI = function(aSpec, aCharset, aBaseURI)
{
    var url =
      Components.classes[STANDARDURL_CONTRACTID].createInstance(nsIStandardURL);
    url.init(nsIStandardURL.URLTYPE_STANDARD, -1, aSpec, aCharset, null);

    return url.QueryInterface(nsIURI);
}

ED2KProtocolHandler.prototype.newChannel = function(aURI)
{
    // rewrite the ed2k URL to a http URL to the mldonkey server
    var myURI = "http://" + cfgUser + ":" + cfgPass + "@" + cfgServer + ":" +
                cfgPort + "/submit?q=dllink+" + aURI.spec;

    // coax the rewritten URL to the http protocol handler
    var ioServ = Components.classesByID[NS_IOSERVICE_CID].getService(nsIIOService);
    //    ioServ = ioServ.QueryInterface(nsIIOService);
    var chan = ioServ.newChannel(myURI, null, null);
    return chan;
}

/***** ED2KProtocolHandlerFactory *****/

var ED2KProtocolHandlerFactory = new Object();

ED2KProtocolHandlerFactory.createInstance = function(outer, iid)
{
    if(outer != null) throw Components.results.NS_ERROR_NO_AGGREGATION;

    if(!iid.equals(nsIProtocolHandler) && !iid.equals(nsISupports))
        throw Components.results.NS_ERROR_INVALID_ARG;

    // get preferences branch
    var PrefService = Components.classes[NS_PREFSERVICE_CONTRACTID].getService(nsIPrefService);
    var myPrefs = PrefService.getBranch(null);  // Bug #107617

    // read preferences (if available)
    if(myPrefs.getPrefType(PREF_BRANCH + "user") == myPrefs.PREF_STRING)
      cfgUser = myPrefs.getCharPref(PREF_BRANCH + "user");
    if(myPrefs.getPrefType(PREF_BRANCH + "pass") == myPrefs.PREF_STRING)
      cfgPass = myPrefs.getCharPref(PREF_BRANCH + "pass");
    if(myPrefs.getPrefType(PREF_BRANCH + "server") == myPrefs.PREF_STRING)
      cfgServer = myPrefs.getCharPref(PREF_BRANCH + "server");
    if(myPrefs.getPrefType(PREF_BRANCH + "port") == myPrefs.PREF_STRING)
      cfgPort = myPrefs.getCharPref(PREF_BRANCH + "port");

    return new ED2KProtocolHandler();
}

/***** Ed2kzillaModule *****/

var Ed2kzillaModule = new Object();

Ed2kzillaModule.registerSelf = function(compMgr, fileSpec, location, type)
{
    // register ed2k protocol handler object
    compMgr =
      compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    dump("*** Registering ed2k protocol handler.\n");
    compMgr.registerFactoryLocation(ED2KPROT_HANDLER_CID,
                                    "ED2K protocol handler",
                                    ED2KPROT_HANDLER_CONTRACTID,
                                    fileSpec, location, type);
}

Ed2kzillaModule.unregisterSelf = function(compMgr, fileSpec, location)
{
    compMgr =
      compMgr.QueryInterface(Components.interfaces.nsIComponentRegistrar);
    compMgr.unregisterFactoryLocation(ED2KPROT_HANDLER, fileSpec);
}

Ed2kzillaModule.getClassObject = function(compMgr, cid, iid)
{
    if(cid.equals(ED2KPROT_HANDLER_CID)) return ED2KProtocolHandlerFactory;

    if(!iid.equals(Components.interfaces.nsIFactory))
        throw Components.results.NS_ERROR_NOT_IMPLEMENTED;

    throw Components.results.NS_ERROR_NO_INTERFACE;
}

Ed2kzillaModule.canUnload = function(compMgr)
{
    return true;    // this object can be unloaded
}

/***** Entrypoint *****/

function NSGetModule(compMgr, fileSpec)
{
    return Ed2kzillaModule;
}
