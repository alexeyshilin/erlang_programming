%% Микроблог, который запускает фрэйм с меню и
%% позволяет вызывать диалоговое окно "about" box
-module(myblog).
-compile(export_all).

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
	loop(Frame,Text,"BLOG"),
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
	wxMenu:append(File,?SAVEAS,"Save as\tCtrl-S"),
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

loop(Frame,Text, Filepath) ->
	receive

		#wx{event=#wxClose{type=close_window}} ->
			io:format("[quit icon]"),
			%wxWindow:close(Frame,[]),
			wxFrame:destroy(Frame);

		#wx{id=?ABOUT, event=#wxCommand{}} ->
			Str = "MyBlog is a minimal WxErlang example.",
			MD = wxMessageDialog:new(Frame,Str, [{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, "About MyBlog"}]),
			wxDialog:showModal(MD),
			wxDialog:destroy(MD),
			loop(Frame,Text,Filepath);

		#wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} ->
			io:format("[exit]"),
			wxWindow:close(Frame,[]);

		#wx{id=?APPEND, event=#wxCommand{type=command_menu_selected}} ->
			Prompt = "Please enter text here.",
			MD = wxTextEntryDialog:new(Frame,Prompt, [{caption, "New blog entry"}]),
			
			case wxTextEntryDialog:showModal(MD) of
				?wxID_OK ->
					Str = wxTextEntryDialog:getValue(MD),
					wxTextCtrl:appendText(Text,[10]++dateNow()++Str);
				_ -> ok
			end,

			wxDialog:destroy(MD),

			loop(Frame,Text,Filepath);

		#wx{id=?UNDO, event=#wxCommand{type=command_menu_selected}} ->
			{StartPos,EndPos} = lastLineRange(Text),
			wxTextCtrl:remove(Text,StartPos-2,EndPos+1),
			loop(Frame,Text,Filepath);

		#wx{id=?OPEN, event=#wxCommand{type=command_menu_selected}} ->
			Filepath = "BLOG",
			wxTextCtrl:loadFile(Text,Filepath),
			loop(Frame,Text,Filepath);

		#wx{id=?OPENFILE, event=#wxCommand{type=command_menu_selected}} ->
			FD = wxFileDialog:new(Frame),
			SelectedFile = case wxFileDialog:showModal(FD) of
				?wxID_OK ->
					wxFileDialog:getPath(FD);
				_ -> ok
			end,
			wxFileDialog:destroy(FD),

			io:format("[~p]", [SelectedFile]),
			wxTextCtrl:loadFile(Text, SelectedFile),

			loop(Frame,Text,SelectedFile);

		#wx{id=?SAVE, event=#wxCommand{type=command_menu_selected}} ->
			wxTextCtrl:saveFile(Text,[{file,Filepath}]),
			loop(Frame,Text,Filepath);

		#wx{id=?SAVEAS, event=#wxCommand{type=command_menu_selected}} ->
			FD = wxFileDialog:new(Frame),
			SelectedFile = case wxFileDialog:showModal(FD) of
				?wxID_OK ->
					wxFileDialog:getPath(FD);
				_ -> ok
			end,
			wxFileDialog:destroy(FD),

			wxTextCtrl:saveFile(Text,[{file,SelectedFile}]),

			loop(Frame,Text,Filepath);

		#wx{id=?NEW, event=#wxCommand{type=command_menu_selected}} ->
			{_,EndPos} = lastLineRange(Text),
			StartPos = wxTextCtrl:xYToPosition(Text,0,0),
			wxTextCtrl:replace(Text,StartPos,EndPos,"MyBlog"),
			loop(Frame,Text,Filepath);
		Any ->
			io:format("[~p]", [Any]),
			loop(Frame,Text,Filepath)
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
