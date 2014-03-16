# API

## What is it?

This API is yet another way to control mldonkey core. It is easier to use compared 
to [Gui protocol][gui] (custom binary format vs json), it is actually designed as a programmable
api compared to telnet interface, which was only meant for human consumption. It is easily
extensible, and will ideally evolve to fully cover all mldonkey capabilities.


Currently API provides ways to control downloads and searches.

## Operation

For access control API piggybacks on existing mldonkey authentication mechanisms, i.e. HTTP basic
authentication with usual mldonkey user credentials.

The API is available over HTTP on mldonkey `http_port`. API endpoint is `/api/1/`, all urls found
below are relative to this root and have `.json` appended to request path.
Parameters are transmitted as HTTP GET parameters. E.g. full url to pause all downloads:

`/api/1/downloads/all/pause.json`

On success API returns HTTP code 200 and specific response in JSON format (see below).
Failed request is indicated with HTTP 404 and error response :

`{"error":"error reason", "backtrace":"exception stack trace (optional)"}`

## Methods

### `/downloads` - get the list of all downloads

*Example response:*

		[
			{
				"id": 1,
				"name": "download name",
				"size": 112376828,
				"downloaded": 65536,
				"download_rate": 0.0,
				"state": "Paused",
				"network": "BT"
			},
			{
				"id": 10,
				"name": "another download name",
				"size": 887807968,
				"downloaded": 29663200,
				"download_rate": 120.0,
				"state": "Downloading",
				"network": "FTP"
			}
		 ]

`download_rate` is in bytes per second. (**FIXME**)

`network`:

* `BT` - Bittorrent
* `ED2K` - Donkey
* `FTP` - HTTP or FTP
* `DC` - DirectConnect
* `GNUT` - Gnutella
* `FT` - Fasttrack
* `SLSK` - Soulseek
* `ON` - OpenNapster

`state = Downloading | Queued | Paused | Downloaded | Shared | Cancelled | New | Aborted`

### `/downloads/start` - start new download(s)

*Parameters:*

* `url`+ - URL specifying a download (1 or more)

*Example response:*

		[
			{
				"url": "matching url parameter",
				"status": "Ok",
				"info":
					[
						"free text additional information"
					]
			}
		]
	
`status` is `Ok` on success, `No_match` if no network plugin could handle this url, `Web` if url designates an
HTTP resource and status of download is not immediately known.

### `/downloads/$SPEC/pausa` - pause specified download(s)

`$SPEC` can be `all` to specify all downloads, or a numerical download id.

*Example response on success:*

		[]

*Example response on failure:*

		{
			"failed":
			[
				{"id": 1, "info": "free text reason"},
				{"id": 10, "info": "free text reason"}
			]
		}


### `/downloads/$SPEC/resume` - resume specified download(s)

idem

### `/downloads/$SPEC/cancel` - cancel specified download(s)

idem

### `/searches`

FIXME

### `/searches/start/remote` or `/searches/start` - start a remote search, `/searches/start/local` - start a local search

*Parameters:*

* `query`

FIXME

### `/searches/$SPEC/forget` - forget specified search(es)

`$SPEC` is either `all` or a numerical search id.

*Response:*

		[]

### `/searches/$ID/results` - get search results

FIXME

### `/results/$ID/download` - start download from given search result

FIXME

---

[gui]: http://mldonkey.sourceforge.net/Gui
