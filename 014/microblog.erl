%% Микроблог, который запускает фрэйм с меню и
%% позволяет вызывать диалоговое окно "about" box
-module(microblog).
-compile(export_all).

-include_lib("wx/include/wx.hrl").

-define(ABOUT,?wxID_ABOUT).
-define(EXIT,?wxID_EXIT).

%% Основная функция: запускает wx-server, создаёт графические объекты,
%% выводит на экран приложения и обрабатывает запрос на завершение
%% приложения и проводит очистку ресурсов.
start() ->
	WX = wx:new(),
	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "MicroBlog"),
	setup(WX,Frame),
	wxFrame:show(Frame),
	loop(Frame),
	wx:destroy().

%% Фрэйм: создаёт панель меню, два подменю, два элемента меню
%% и панель статуса. Соединяет фрэйм для обработки событий.
setup(WX,Frame) ->
	MenuBar = wxMenuBar:new(),
	File = wxMenu:new(),
	Help = wxMenu:new(),
	
	wxMenu:append(Help,?ABOUT,"About MicroBlog"),
	wxMenu:append(File,?EXIT,"Quit"),

	wxMenuBar:append(MenuBar,File,"&File"),
	wxMenuBar:append(MenuBar,Help,"&Help"),
	
	wxFrame:setMenuBar(Frame,MenuBar),

	wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame,"Welcome to wxErlang"),

	wxFrame:connect(Frame, command_menu_selected),
	wxFrame:connect(Frame, close_window).

loop(Frame) ->
	receive
		#wx{id=?ABOUT, event=#wxCommand{}} ->
			Str = "MicroBlog is a minimal WxErlang example.",
			MD = wxMessageDialog:new(Frame,Str, [{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, "About MicroBlog"}]),
			wxDialog:showModal(MD),
			wxDialog:destroy(MD),
			loop(Frame);
		#wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} ->
			wxWindow:close(Frame,[])
	end.

% c(microblog).
%
% microblog:start().