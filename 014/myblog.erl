%% Микроблог, который запускает фрэйм с меню и
%% позволяет вызывать диалоговое окно "about" box
-module(myblog).
-compile(export_all).

-include("blog.hrl").

-include_lib("wx/include/wx.hrl").

-define(ABOUT,?wxID_ABOUT).
-define(EXIT,?wxID_EXIT).
-define(APPEND, 131).
-define(UNDO, 132).
-define(OPEN, 133).
-define(SAVE, 134).
-define(NEW, 135).
-define(OPENFILE, 136).
-define(SAVEAS, 137).
-define(CLOSE, 138).
-define(NEWTAB, 139).
-define(NEWFILE, 140).

%% Основная функция: запускает wx-server, создаёт графические объекты,
%% выводит на экран приложения и обрабатывает запрос на завершение
%% приложения и проводит очистку ресурсов.

start() ->
	WX = wx:new(),
	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "MyBlog"),
	%{H, W} = wxFrame:getBestSize(Frame),
	Panel = wxPanel:new(Frame, []),

	%Sizer = wxBoxSizer:new(?wxEXPAND), % ?wxVERTICAL ?wxHORIZONTAL ?wxALL ?wxEXPAND
	%wxBoxSizer:setMinSize(Sizer,250, 200),
	Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Some info"}]),
	wxStaticBoxSizer:setMinSize(Sizer,300, 200),

	%SomeOptions = [{flag, ?wxALL bor ?wxEXPAND}],
	%wxBoxSizer:add(Sizer, Panel, SomeOptions),

	%SomeOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],
	%wxStaticBoxSizer:add(Sizer, Panel, SomeOptions),


	wxPanel:setSizer(Panel, Sizer),

	%Notebook = wxNotebook:new(Panel, ?wxID_ANY, [{size, {-1, -1}}]),
	Notebook = wxNotebook:new(Panel, ?wxID_ANY, [{size, {500, 400}}]),
	%wxNotebook:fit(Notebook),
	%wxNotebook:setSize(Notebook, {H, W}),
	%{H, W} = wxNotebook:getBestSize(Notebook),
	%wxNotebook:setSizer(Notebook, Sizer),

	Win1 = wxPanel:new(Notebook, [{style, ?wxBK_DEFAULT}]), % style: ?wxBK_ALIGN_MASK, ?wxBK_TOP, ?wxBK_BOTTOM, ?wxBK_LEFT, ?wxBK_RIGHT, ?wxNB_MULTILINE
	%wxWindow:fit(Win1),
	%wxWindow:getBestSize(Win1),
	%wxWindow:setSize(Win1, {H, W}),
	wxNotebook:addPage(Notebook, Win1, "Tab one", []),

	Text = wxHtmlWindow:new(Win1, [{id, ?wxID_ANY}, {style,?wxTE_MULTILINE}, {size, {300,200}}]),

	Tabs = [{Win1, Text, "BLOG", null, []}], % win text filename db posts

	setup(WX,Frame,Text, Notebook),
	wxFrame:show(Frame),
	loop(Frame,Text,"BLOG", null, [], Notebook, Tabs),
	wx:destroy().

%% Фрэйм: создаёт панель меню, два подменю, два элемента меню
%% и панель статуса. Соединяет фрэйм для обработки событий.
setup(WX, Frame, Text, Notebook) ->
	MenuBar = wxMenuBar:new(),
	File = wxMenu:new(),
	Help = wxMenu:new(),
	Edit = wxMenu:new(),
	
	wxMenu:append(Help,?ABOUT,"About MyBlog"),
	wxMenu:append(File,?EXIT,"Quit"),

	wxMenuBar:append(MenuBar,File,"&File"),
	wxMenuBar:append(MenuBar,Help,"&Help"),
	
	wxMenu:append(File,?NEW,"New\tCtrl-N"),
	wxMenu:append(File,?OPEN,"Open saved\tCtrl-0" ) ,
	wxMenu:append(File,?OPENFILE,"Open file\tCtrl-o" ) ,
	wxMenu:append(File,?NEWFILE,"Create db file\tCtrl-n" ) ,
	wxMenu:appendSeparator(File),
	wxMenu:append(File,?SAVE,"Save\tCtrl-S"),
	%wxMenu:append(File,?SAVEAS,"Save as\tCtrl-S"),
	wxMenu:appendSeparator(File),
	wxMenu:append(File,?NEWTAB,"New tab\tCtrl-N" ) ,
	wxMenu:append(File,?CLOSE,"Close\tCtrl-C" ) ,

	wxMenu:append(Edit,?APPEND,"Add en&try\tCtrl-T" ) ,
	wxMenu:append(Edit,?UNDO,"Undo latest\tCtrl-U" ) ,

	wxMenuBar:append(MenuBar,Edit,"&Edit"),

	wxFrame:setMenuBar(Frame,MenuBar),

	wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame,"Welcome to wxErlang"),

	%wxHtmlWindow:setEditable(Text,false),

	wxFrame:connect(Frame, command_menu_selected),
	wxFrame:connect(Frame, close_window,  [{skip, true}]),
	ok.

is_file_exists(Filename)->
	case file:read_file_info(Filename) of
		{ok, FileInfo} -> true;
		{error, Reason} -> false
	end.

data_as_text([])->
	"";

data_as_text([H|[]])->
	{Id,Date,Message} = H,
	date(Date) ++ " " ++ Message ++ "\n";

data_as_text([H|T])->
	{Id,Date,Message} = H,
	date(Date) ++ " " ++ Message ++ "\n" ++ data_as_text(T);

data_as_text(Any)->
	{error, data_as_text}.

% save_new_posts
save_new_posts(Db, [])->
	ok;

save_new_posts(Db, [H|[]])->
	{Id,Stamp,Message} = H,
	blog_db:add_item(Db, Message, Stamp);

save_new_posts(Db, [H|T])->
	{Id,Stamp,Message} = H,
	blog_db:add_item(Db, Message, Stamp),
	save_new_posts(Db, T);

save_new_posts(Db, Any)->
	{error, save_new_posts}.
% /save_new_posts

% remove_tab
remove_tab([H|[]], P, C) when P==C ->
	[];
remove_tab([H|[]], P, C) when not(P==C) ->
	[H];

remove_tab([H|T], P, C) when P==C ->
	T;
remove_tab([H|T], P, C) when not(P==C) ->
	[H]++remove_tab(T,P,C+1);

remove_tab(L, P, C) ->
	{error, remove_tab}.
% /remove_tab

% get_tab_data
get_tab_data([H|[]], P, C) when P==C ->
	H;
get_tab_data([H|[]], P, C) when not(P==C) ->
	null;

get_tab_data([H|T], P, C) when P==C ->
	H;
get_tab_data([H|T], P, C) when not(P==C) ->
	get_tab_data(T,P,C+1);

get_tab_data(L, P, C) ->
	{error, get_tab_data}.
% /get_tab_data

% set_tab_data
set_tab_data([H|[]], P, C, TD) when P==C ->
	[TD];
set_tab_data([H|[]], P, C, TD) when not(P==C) ->
	[H];

set_tab_data([H|T], P, C, TD) when P==C ->
	[TD]++T;
set_tab_data([H|T], P, C, TD) when not(P==C) ->
	[H]++set_tab_data(T,P,C+1, TD);

set_tab_data(L, P, C, TD) ->
	{error, set_tab_data}.
% /set_tab_data

loop(Frame,Text, Filepath, Db, Posts, Notebook, Tabs) ->
	receive

		#wx{event=#wxClose{type=close_window}} ->
			io:format("[quit icon]"),
			blog_db:close(Db),
			%wxWindow:close(Frame,[]),
			wxFrame:destroy(Frame);

		#wx{id=?ABOUT, event=#wxCommand{}} ->
			Str = "MyBlog is a minimal WxErlang example.",
			MD = wxMessageDialog:new(Frame,Str, [{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, "About MyBlog"}]),
			wxDialog:showModal(MD),
			wxDialog:destroy(MD),
			loop(Frame,Text,Filepath, Db, Posts, Notebook, Tabs);

		#wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} ->
			io:format("[exit]"),
			blog_db:close(Db),
			wxWindow:close(Frame,[]);

		#wx{id=?APPEND, event=#wxCommand{type=command_menu_selected}} ->
			Prompt = "Please enter text here.",
			MD = wxTextEntryDialog:new(Frame,Prompt, [{caption, "New blog entry"}, {style, ?wxTE_MULTILINE bor ?wxOK bor ?wxCANCEL bor ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}]), % ?wxOK | ?wxCANCEL | ?wxTE_MULTILINE
			
			N = wxNotebook:getSelection(Notebook),
			{WinCur, TextCur, FilepathCur, DbRefCur, PostsCur} = get_tab_data(Tabs, N, 0),

			case wxTextEntryDialog:showModal(MD) of
				?wxID_OK ->
					Str = wxTextEntryDialog:getValue(MD),
					%blog_db:add_item(DbRefCur, Str),
					%Now = calendar:now_to_datetime(erlang:now()),
					Now = blog_db:unixtime(),
					NewPosts = PostsCur ++ [{0, Now, Str}],
					wxHtmlWindow:appendToPage(TextCur,[10]++dateNow()++" "++Str),

					NewTab = {WinCur, TextCur, FilepathCur, DbRefCur, NewPosts},
					NewTabs = set_tab_data(Tabs, N, 0, NewTab),
					ok;

				_ -> NewPosts = PostsCur, NewTabs=Tabs, ok
			end,

			wxDialog:destroy(MD),

			loop(Frame,Text,Filepath, Db, NewPosts, Notebook, NewTabs);

		#wx{id=?UNDO, event=#wxCommand{type=command_menu_selected}} ->
			{StartPos,EndPos} = lastLineRange(Text),
			wxHtmlWindow:remove(Text,StartPos-2,EndPos+1),
			loop(Frame,Text,Filepath, Db, Posts, Notebook, Tabs);

		#wx{id=?OPEN, event=#wxCommand{type=command_menu_selected}} ->
			Filepath = "BLOG",
			%wxHtmlWindow:loadFile(Text,Filepath),

			{ok, Ref} = case is_file_exists(Filepath) of
						true -> blog_db:open(Filepath);
						false -> blog_db:create(Filepath), blog_db:open(Filepath)
					end,

			Data = blog_db:get_simple(Ref),
			TextData = data_as_text(Data),
			wxHtmlWindow:changeValue(Text, TextData),

			loop(Frame,Text,Filepath, Ref, Posts, Notebook, Tabs);

		#wx{id=?OPENFILE, event=#wxCommand{type=command_menu_selected}} ->
			FD = wxFileDialog:new(Frame),
			SelectedFile = case wxFileDialog:showModal(FD) of
				?wxID_OK ->
					wxFileDialog:getPath(FD);
				_ -> ok
			end,
			wxFileDialog:destroy(FD),

			io:format("[~p]", [SelectedFile]),
			%wxHtmlWindow:loadFile(Text, SelectedFile),

			{ok, Ref} = case is_file_exists(SelectedFile) of
						true -> blog_db:open(SelectedFile);
						false -> blog_db:create(SelectedFile), blog_db:open(SelectedFile)
					end,

			Data = blog_db:get_simple(Ref),
			TextData = data_as_text(Data),

			N = wxNotebook:getSelection(Notebook),
			{WinCur, TextCur, FilepathCur, DbRefCur, PostsCur} = get_tab_data(Tabs, N, 0),
			NewTabs = set_tab_data(Tabs, N, 0, {WinCur, TextCur, FilepathCur, Ref, PostsCur}),

			wxHtmlWindow:setPage(TextCur, TextData),

			loop(Frame,Text,SelectedFile, Ref, Posts, Notebook, NewTabs);

		#wx{id=?NEWFILE, event=#wxCommand{type=command_menu_selected}} ->
			Prompt = "Please enter filename here.",
			MD = wxTextEntryDialog:new(Frame,Prompt, [{caption, "New file name"}]),

			Filename = case wxTextEntryDialog:showModal(MD) of
				?wxID_OK -> wxTextEntryDialog:getValue(MD);
				_ -> null
			end,

			wxDialog:destroy(MD),

			Res = case is_file_exists(Filename) of
						true -> {error, file_already_exists};
						false -> blog_db:create(Filename)
					end,

			loop(Frame,Text,Filepath, Db, Posts, Notebook, Tabs);

		#wx{id=?SAVE, event=#wxCommand{type=command_menu_selected}} ->
			%wxHtmlWindow:saveFile(Text,[{file,Filepath}]),

			N = wxNotebook:getSelection(Notebook),
			{WinCur, TextCur, FilepathCur, DbRefCur, PostsCur} = get_tab_data(Tabs, N, 0),
			io:format("[~p]", [PostsCur]),
			save_new_posts(DbRefCur, PostsCur),
			NewTabs = set_tab_data(Tabs, N, 0, {WinCur, TextCur, FilepathCur, DbRefCur, []}),

			loop(Frame,Text,Filepath, Db, Posts, Notebook, NewTabs);

		#wx{id=?SAVEAS, event=#wxCommand{type=command_menu_selected}} ->
			FD = wxFileDialog:new(Frame),
			SelectedFile = case wxFileDialog:showModal(FD) of
				?wxID_OK ->
					wxFileDialog:getPath(FD);
				_ -> ok
			end,
			wxFileDialog:destroy(FD),

			wxHtmlWindow:saveFile(Text,[{file,SelectedFile}]),

			loop(Frame,Text,Filepath, Db, Posts, Notebook, Tabs);

		#wx{id=?NEW, event=#wxCommand{type=command_menu_selected}} ->
			{_,EndPos} = lastLineRange(Text),
			StartPos = wxHtmlWindow:xYToPosition(Text,0,0),
			wxHtmlWindow:replace(Text,StartPos,EndPos,"MyBlog"),
			loop(Frame,Text,Filepath, Db, Posts, Notebook, Tabs);

		#wx{id=?NEWTAB, event=#wxCommand{type=command_menu_selected}} ->

			Win2 = wxPanel:new(Notebook, [{style, ?wxBK_DEFAULT}]),
			%wxWindow:fit(Win2),
			%wxWindow:getBestSize(Win2),
			%wxWindow:setSize(Win2, {H, W}),

			TxtLabel = lists:flatten(io_lib:format("Tab ~p", [length(Tabs)+1])),
			wxNotebook:addPage(Notebook, Win2, TxtLabel, []),

			Text2 = wxHtmlWindow:new(Win2, [{id, ?wxID_ANY}, {style,?wxTE_MULTILINE}, {size, {300,200}}]),

			NewTabs = Tabs ++ [{Win2, Text2, null, null, []}],

			loop(Frame,Text,Filepath, Db, Posts, Notebook, NewTabs);

		#wx{id=?CLOSE, event=#wxCommand{type=command_menu_selected}} ->

			%N = wxNotebook:getPageCount(Notebook),
			%io:format("[~p]", [N]),

			%{Ref,Id,wxWindow,_} = wxNotebook:getCurrentPage(Notebook),

			case wxNotebook:getSelection(Notebook) of
				 not_found ->
					NewTabs = Tabs,
					ok;
				 N ->
					NewTabs = remove_tab(Tabs, N, 0),
					wxNotebook:removePage(Notebook, N),
					%io:format("[~p]", [length(NewTabs)]),
					ok
			end,

			loop(Frame,Text,Filepath, Db, Posts, Notebook, NewTabs);

		Any ->
			io:format("[~p]", [Any]),
			loop(Frame,Text,Filepath, Db, Posts, Notebook, Tabs)
	end.

dateNow()->
	Now = calendar:now_to_datetime(erlang:now()),
	date(Now).

date({{Year, Month, Day}, {Hour, Minute, Second}})->
	StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
	StrTime.

lastLineRange(T) ->
	S = wxHtmlWindow:xYToPosition(T,0,wxHtmlWindow:getNumberOfLines(T)-1),
	F = wxHtmlWindow:getLastPosition(T),
	{S,F}.

% c(blog_db).
% c(myblog).
%
% myblog:start().
