
open DriverInteractive
open GuiTypes

let of_file f =
  let f = CommonFile.file_info f in
  {
    DriverApi_t.file_id = f.file_num;
    file_name = f.file_name;
    file_size = f.file_size;
    file_downloaded = f.file_downloaded;
    file_download_rate = f.file_download_rate;
    file_network = net_name f;
    file_state = f.file_state;
  }

let of_search { CommonTypes.search_num; search_string; search_waiting; search_nresults; } =
    { DriverApi_t.search_id = search_num;
      search_query = search_string;
      search_waiting = search_waiting;
      search_results = search_nresults;
    }
