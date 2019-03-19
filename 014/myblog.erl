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

%% Основная функция: запускает wx-server, создаёт графические объекты,
%% выводит на экран приложения и обрабатывает запрос на завершение
%% приложения и проводит очистку ресурсов.

start() ->
	WX = wx:new(),
	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "MyBlog"),
	Text = wxTextCtrl:new(Frame, ?wxID_ANY, [{value,"MyBlog"}, {style,?wxTE_MULTILINE}]),
	setup(WX,Frame,Text),
	wxFrame:show(Frame),
	loop(Frame,Text,"BLOG", null, []),
	wx:destroy().

%% Фрэйм: создаёт панель меню, два подменю, два элемента меню
%% и панель статуса. Соединяет фрэйм для обработки событий.
setup(WX, Frame, Text) ->
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
	wxMenu:appendSeparator(File),
	wxMenu:append(File,?SAVE,"Save\tCtrl-S"),
	%wxMenu:append(File,?SAVEAS,"Save as\tCtrl-S"),
	wxMenu:append(Edit,?APPEND,"Add en&try\tCtrl-T" ) ,
	wxMenu:append(Edit,?UNDO,"Undo latest\tCtrl-U" ) ,
	wxMenuBar:append(MenuBar,Edit,"&Edit"),

	wxFrame:setMenuBar(Frame,MenuBar),

	wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame,"Welcome to wxErlang"),

	wxTextCtrl:setEditable(Text,false),

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

loop(Frame,Text, Filepath, Db, Posts) ->
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
			loop(Frame,Text,Filepath, Db, Posts);

		#wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} ->
			io:format("[exit]"),
			blog_db:close(Db),
			wxWindow:close(Frame,[]);

		#wx{id=?APPEND, event=#wxCommand{type=command_menu_selected}} ->
			Prompt = "Please enter text here.",
			MD = wxTextEntryDialog:new(Frame,Prompt, [{caption, "New blog entry"}]),
			
			case wxTextEntryDialog:showModal(MD) of
				?wxID_OK ->
					Str = wxTextEntryDialog:getValue(MD),
					%blog_db:add_item(Db, Str),
					%Now = calendar:now_to_datetime(erlang:now()),
					Now = blog_db:unixtime(),
					NewPosts = Posts ++ [{0, Now, Str}],
					wxTextCtrl:appendText(Text,[10]++dateNow()++" "++Str);
				_ -> NewPosts = Posts, ok
			end,

			wxDialog:destroy(MD),

			loop(Frame,Text,Filepath, Db, NewPosts);

		#wx{id=?UNDO, event=#wxCommand{type=command_menu_selected}} ->
			{StartPos,EndPos} = lastLineRange(Text),
			wxTextCtrl:remove(Text,StartPos-2,EndPos+1),
			loop(Frame,Text,Filepath, Db, Posts);

		#wx{id=?OPEN, event=#wxCommand{type=command_menu_selected}} ->
			Filepath = "BLOG",
			%wxTextCtrl:loadFile(Text,Filepath),

			{ok, Ref} = case is_file_exists(Filepath) of
						true -> blog_db:open(Filepath);
						false -> blog_db:create(Filepath), blog_db:open(Filepath)
					end,

			Data = blog_db:get_simple(Ref),
			TextData = data_as_text(Data),
			wxTextCtrl:changeValue(Text, TextData),

			loop(Frame,Text,Filepath, Ref, Posts);

		#wx{id=?OPENFILE, event=#wxCommand{type=command_menu_selected}} ->
			FD = wxFileDialog:new(Frame),
			SelectedFile = case wxFileDialog:showModal(FD) of
				?wxID_OK ->
					wxFileDialog:getPath(FD);
				_ -> ok
			end,
			wxFileDialog:destroy(FD),

			io:format("[~p]", [SelectedFile]),
			%wxTextCtrl:loadFile(Text, SelectedFile),

			{ok, Ref} = case is_file_exists(SelectedFile) of
						true -> blog_db:open(SelectedFile);
						false -> blog_db:create(SelectedFile), blog_db:open(SelectedFile)
					end,

			Data = blog_db:get_simple(Ref),
			TextData = data_as_text(Data),
			wxTextCtrl:changeValue(Text, TextData),

			loop(Frame,Text,SelectedFile, Ref, Posts);

		#wx{id=?SAVE, event=#wxCommand{type=command_menu_selected}} ->
			%wxTextCtrl:saveFile(Text,[{file,Filepath}]),
			save_new_posts(Db, Posts),
			loop(Frame,Text,Filepath, Db, Posts);

		#wx{id=?SAVEAS, event=#wxCommand{type=command_menu_selected}} ->
			FD = wxFileDialog:new(Frame),
			SelectedFile = case wxFileDialog:showModal(FD) of
				?wxID_OK ->
					wxFileDialog:getPath(FD);
				_ -> ok
			end,
			wxFileDialog:destroy(FD),

			wxTextCtrl:saveFile(Text,[{file,SelectedFile}]),

			loop(Frame,Text,Filepath, Db, Posts);

		#wx{id=?NEW, event=#wxCommand{type=command_menu_selected}} ->
			{_,EndPos} = lastLineRange(Text),
			StartPos = wxTextCtrl:xYToPosition(Text,0,0),
			wxTextCtrl:replace(Text,StartPos,EndPos,"MyBlog"),
			loop(Frame,Text,Filepath, Db, Posts);
		Any ->
			io:format("[~p]", [Any]),
			loop(Frame,Text,Filepath, Db, Posts)
	end.

dateNow()->
	Now = calendar:now_to_datetime(erlang:now()),
	date(Now).

date({{Year, Month, Day}, {Hour, Minute, Second}})->
	StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
	StrTime.

lastLineRange(T) ->
	S = wxTextCtrl:xYToPosition(T,0,wxTextCtrl:getNumberOfLines(T)-1),
	F = wxTextCtrl:getLastPosition(T),
	{S,F}.

% c(myblog).
%
% myblog:start().
