unit UnitChatStatisticsSub;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, Vcl.Controls, WEBLib.JSON,
  WEBLib.ExtCtrls,System.DateUtils, System.StrUtils, jsDelphiSystem;

type
  TChatStatisticsSubForm = class(TWebForm)
    btnChatSend: TWebButton;
    memoChat: TWebMemo;
    WebTimer1: TWebTimer;
    [async] procedure WebFormCreate(Sender: TObject);
    [async] procedure btnChatSendClick(Sender: TObject);
    [async] procedure AddChatResponse(ChatMessage: String; QueryTime:TDateTime; ResponseStatus: String; ResponseIcon: String);
    procedure AddStaticChatResponse(ChatMessage: String);
    procedure AddChatMessage(ChatMessage: String);
    procedure memoChatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WebTimer1Timer(Sender: TObject);
    procedure InitializeChat;
    procedure WebFormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ChatContext: JSValue;
    ChatID: String;
  end;

var
  ChatStatisticsSubForm: TChatStatisticsSubForm;

implementation

uses
  UnitMain, UnitIcons;

{$R *.dfm}

procedure TChatStatisticsSubForm.AddChatMessage(ChatMessage: String);
var
  ChatBlock: String;
begin
  ChatBlock := '<div class="direct-chat-msg end">'+
               '  <div class="direct-chat-infos clearfix">'+
               '    <span class="direct-chat-name float-end">'+MainForm.User_FirstName+' '+MainForm.User_LastName+'</span>'+
               '    <span class="direct-chat-timestamp float-start">'+FormatDateTime('yyyy-MMM-dd hh:nn:ss',Now)+'</span>'+
               '  </div>'+
               '  <img class="direct-chat-img" '+Copy(MainForm.User_Photo,5,length(MainForm.User_Photo))+'</img>'+
               '  <div class="direct-chat-text fs-6 cursor-pointer chatmessage">'+
                    ChatMessage+
               '  </div>'+
               '</div>';
  asm
    ChatWindow.innerHTML += ChatBlock;
    ChatWindow.addEventListener('click', (e) => {
      if (e.target.classList.contains('chatmessage')) {
        e.stopImmediatePropagation();
        memoChat.value = e.target.innerHTML.trim();
        memoChat.style.height = '';
        memoChat.style.height = memoChat.scrollHeight+3 +'px';
        memoChat.focus();
        memoChat.selectionStart = memoChat.value.length;
      }
    });
    ChatWindow.scrollTo({left: 0, top: 1000000, behaviour: "smooth"});
  end;

  asm
    this.ChatContext.push({"role":"user","content":JSON.stringify(ChatMessage)});
  end;
end;

procedure TChatStatisticsSubForm.AddChatResponse(ChatMessage: String; QueryTime:TDateTime; ResponseStatus: String; ResponseIcon: String);
var
  ChatBlock: String;
  FormattedMessage: String;
  PreReplace: String;
begin
  FormattedMessage := ChatMessage;

  // Replace ``` with <p><pre> and </pre></p>
  PreReplace := '<p><pre>';
  while pos('```',FormattedMessage) > 0 do
  begin
    FormattedMessage := StringReplace(FormattedMessage,'```',PreReplace,[]);
    if PreReplace = '<p><pre>'
    then PreReplace := '</pre></p>'
    else PreReplace := '<p><pre>';
  end;

  // Replace ` with <pre> and </pre>
  PreReplace := '<pre>';
  while pos('`',FormattedMessage) > 0 do
  begin
    FormattedMessage := StringReplace(FormattedMessage,'`',PreReplace,[]);
    if PreReplace = '<pre>'
    then PreReplace := '</pre>'
    else PreReplace := '<pre>';
  end;

  FormattedMessage := StringReplace(FormattedMessage,'\n','',[rfReplaceAll]);
  if Copy(FormattedMessage,1,1) = '"'
  then FormattedMessage := Copy(FormattedMessage,2,Length(FormattedMessage)-2);

  ChatBlock := '<div class="direct-chat-msg">'+
               '  <div class="direct-chat-infos clearfix">'+
               '    <span class="direct-chat-name float-start">'+MainForm.App_ChatBotName+'</span>'+
               '    <span class="direct-chat-timestamp float-end">'+
                      '('+FloatToStrF(MillisecondsBetween(QueryTime,Now)/1000,ffNumber,5,2)+'s) '+
                      '<span title="'+ResponseStatus+'">'+ResponseIcon+'</span>'+
                      FormatDateTime(' yyyy-MMM-dd hh:nn:ss',Now)+
                    '</span>'+
               '  </div>'+
                  DMIcons.Icon('Robot_Avatar')+
               '  <div class="direct-chat-text fs-6">'+
               '  </div>'+
               '</div>';
  asm
    ChatWindow.innerHTML += ChatBlock;
    var ChatMessageWords = FormattedMessage.split(' ');
    for (var i = 1; i <= ChatMessageWords.length; i++) {
      ChatWindow.lastElementChild.lastElementChild.innerHTML = ChatMessageWords.slice(0,i).join(' ');
      ChatWindow.scrollTo({left: 0, top: 1000000, behaviour: "smooth"});
      await new Promise(r => setTimeout(r, Math.random() * 200));
    }
  end;

  // Don't do this for images, just regular conversations
  if (Pos('<img ',ChatMessage) <> 1) then
  begin
    asm
      var ascii = '';
      for (var i = 0; i < ChatMessage.length; i++) {
        var charCode = ChatMessage.charCodeAt(i);
        if (charCode < 128) {
          ascii += String.fromCharCode(charCode);
       }
      }
      this.ChatContext.push({"role":"assistant","content":JSON.stringify(ascii)});
    end;
  end;
end;

procedure TChatStatisticsSubForm.AddStaticChatResponse(ChatMessage: String);
var
  ChatBlock: String;
begin
  ChatBlock := '<div class="direct-chat-msg">'+
               '  <div class="direct-chat-infos clearfix">'+
               '    <span class="direct-chat-name float-start">'+MainForm.App_ChatBotName+'</span>'+
               '    <span class="direct-chat-timestamp float-end">'+FormatDateTime('yyyy-MMM-dd hh:nn:ss',Now)+'</span>'+
               '  </div>'+
               DMIcons.Icon('Robot_Avatar')+
               '  <div class="direct-chat-text fs-6">'+
                    ChatMessage+
               '  </div>'+
               '</div>';
  asm
    ChatWindow.innerHTML += ChatBlock;
    ChatWindow.scrollTo({left: 0, top: 1000000, behaviour: "smooth"});
  end;
end;

procedure TChatStatisticsSubForm.btnChatSendClick(Sender: TObject);
var
  i: Integer;
  MessageString: String;
  ResponseString: String;
  ResponseJSON: TJSONObject;
  QueryTime: TDateTime;
  Model: String;
  Choices: String;
  Context: String;
  ResponseStatus: String;
  ResponseIcon: String;
  Images: String;
begin

  // Get a string that has \n for line endings
  MessageString := '';
  for i := 0 to memoChat.Lines.Count -1 do
  begin
    if Trim(memoChat.lines[i]) <> ''
    then MessageString := MessageString + Trim(memoChat.lines[i])+ ' \n';
  end;
  if RightStr(MessageString,3) = ' \n'
  then MessageString := Copy(MessageString,1,length(MessageString) - 3);

  // If we've got something to send, then let's send it
  if (Length(MessageString)) >= 2 then
  begin

    // Add new request to chat window
    AddChatMessage(memoChat.Lines.Text);

    // Clear out text and get ready for next message entry
    memoChat.Lines.Text := '';
    memoChat.SetFocus;
    memoChat.ElementHandle.style.setProperty('height','38px');
    asm
      setTimeout(function() {
        memoChat.value = '';
        memoChat.style.height = '38px';
      },100);
    end;

    // Add a "typing" indicator
    AddStaticChatResponse('<div class="dot-collision"></div>');

    // Get values to pass into appropriate formats
    Model := '';
    Choices := '';
    Context := 'None';
    asm
      MessageString = JSON.stringify(MessageString);
      Model = ChatModelSelection.innerHTML;
      Choices = ChatCountSelection.innerHTML;
      // Get our array but without the opening and closing []
      Context = JSON.stringify(this.ChatContext).slice(1,-1);
    end;

    console.log('Model: '+Model);
    console.log('Choices: '+IntToStr(StrToIntDef(Trim(RightStr(Trim(Choices),2)),1)));
    console.log('Message: '+MessageString);

    // Refresh the ChatID each time for images
    if Pos('IMAGE', Uppercase(Model)) > 0
    then ChatID := MainForm.User_Account+'-'+FormatDateTime('yyyyMMddHHnnsszzz',Now);


    // Submit request
    QueryTime := Now;
    ResponseString := await(MainForm.JSONRequest('IChatService.Chat',[
      Trim(Model), // Model
      MessageString, // Conversation
      Context, // Context
      StrToIntDef(Trim(RightStr(Trim(Choices),2)),1),
      ChatID  // ChatID
    ]));

    asm console.log(JSON.parse(ResponseString)); end;

    // Remove the "typing" indicator
    asm ChatWindow.lastElementChild.remove(); end;

    // Check that we've got something we can use
    if ResponseString <> '' then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(ResponseString) as TJSONObject;

      // See if our request was altered in some way
      ResponseStatus := 'Ok';
      ResponseIcon := DMIcons.Icon('Check');

      if (ResponseJSON <> nil) and (ResponseJSON.GetValue('Trim Message') <> nil) then
      begin
        ResponseStatus := (ResponseJSON.GetValue('Trim Message') as TJSONString).Value;
        if Pos('Clip',ResponseStatus) > 0
        then ResponseIcon := DMIcons.Icon('Scissors');
      end;

      // Long way to check that we've got JSON in an expected structure
      if (ResponseJSON <> nil) and
         ((ResponseJSON.GetValue('choices')) <> nil) and
         ((ResponseJSON.GetValue('choices') as TJSONArray)[0] <> nil) and
         (((ResponseJSON.GetValue('choices') as TJSONArray)[0] as TJSONObject).GetValue('message') <> nil) and
         ((((ResponseJSON.GetValue('choices') as TJSONArray)[0] as TJSONObject).GetValue('message') as TJSONObject).GetValue('content')  <> nil) then
      begin
        // Add response to the ChatWindow
        AddChatResponse(((((ResponseJSON.GetValue('choices') as TJSONArray)[0] as TJSONObject).GetValue('message') as TJSONObject).GetValue('content') as TJSONString).value, QueryTime, ResponseStatus, ResponseIcon);
      end
      else
      begin
        if (ResponseJSON <> nil) and
           ((ResponseJSON.GetValue('data')) <> nil) then
        begin
          Images := '';
          for i := 0 to (ResponseJSON.GetValue('data') as TJSONArray).Count - 1 do
          begin
            Images := Images +'<img title='+MessageString+' width="150" height="auto" class="m-1 rounded cursor-pointer ViewableImage" src="data:image/png;base64,'+(((ResponseJSON.GetValue('data') as TJSONArray).Items[i] as TJSONObject).GetValue('b64_json') as TJSONString).value+'">';
          end;
          AddChatResponse(Images, QueryTime, ResponseStatus, ResponseIcon);
        end
        else
        begin
          AddStaticChatResponse('Sorry, an error was encountered trying to process this request.');
        end
      end;
    end;

  end;
end;

procedure TChatStatisticsSubForm.InitializeChat;
var
  Openers: Array[0..9] of String;
begin
  // Clear out any prior chat
  asm
    ChatWindow.replaceChildren();
  end;

  // Initialize with a canned response of some kind.
  Openers[0] := 'Hello!  My name is '+Mainform.App_ChatBotName+'.  Please ask me anything you like.';
  Openers[1] := 'Greetings and salutations.  It is I, '+Mainform.App_ChatBotName+'.  How may I be of assistance?';
  Openers[2] :=  Mainform.App_ChatBotName+' here. How can I help you?';
  Openers[3] := 'I come in peace. Hahaha. Just kidding. What do you want to know?';
  Openers[4] := 'Bleep. Bleep. Bloop. Bleep.';
  // Bicentennial Man
  Openers[5] := 'One is glad to be of service.';
  // Aladdin
  Openers[6] := 'The ever impressive, the long contained, often imitated, but never duplicated... Genie of the lamp!';
  // Terminator 2
  Openers[7] := 'I have detailed files.';
  // The Matrix
  Openers[8] := 'I am the Architect. I created the matrix. I''ve been waiting for you. You have many questions, and although the process has altered your consciousness, you remain irrevocably human. Ergo, some of my answers you will '+
                'understand, and some of them you will not. Concordantly, while your first question may be the most pertinent, you may or may not realize it is also irrelevant.';
  // Aliens
  Openers[9] := 'Well, that explains it then. The A2s always were a bit twitchy. That could never happen now with our behavioral inhibitors. It is impossible for me to harm or by omission of action, allow to be harmed, a human being.';

  AddStaticChatResponse(Openers[System.Random(10)]);

  asm
    this.ChatContext = [];
  end;

  ChatID := MainForm.User_Account+'-'+FormatDateTime('yyyyMMddHHnnsszzz',Now);
end;

procedure TChatStatisticsSubForm.memoChatKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (key = VK_RETURN) and (shift = []) then btnChatSendClick(Sender);
end;

procedure TChatStatisticsSubForm.WebFormCreate(Sender: TObject);
var
  ResponseString: String;
  Here: JSValue;
begin

  // Summary Info Icons
  asm
    var Icon = pas.UnitIcons.DMIcons.Lookup;
    ChatUsage.innerHTML = Icon['Chat'];
    ImageUsage.innerHTML = Icon['Image'];
    CombinedUsage.innerHTML = Icon['ChatImage'];
  end;

  // Adjustments to the chat input memo field
  memoChat.ElementHandle.setAttribute('rows','1');
  memoChat.ElementHandle.style.setProperty('min-height','38px');
  memoChat.ElementHandle.style.setProperty('max-height','250px');
  btnChatSend.Caption := DMIcons.icon('Send')+'Send';

  // Chat Window adjustments
  asm
    ChatWindow.style.setProperty('height','auto');
    ChatWindow.style.setProperty('min-height','300px');
    ChatWindow.style.setProperty('max-height',(divSubForm.offsetHeight-321)+'px');
    Recents.style.setProperty('max-height',(divSubForm.offsetHeight-275)+'px');
    ImageRecent.style.setProperty('min-height','285px');

    ChatWindow.addEventListener('click', (e) => {
      if (e.target.classList.contains('ViewableImage')) {
        e.stopImmediatePropagation();
        pas.UnitMain.MainForm.Viewer(e.target.outerHTML);
      }
    });
  end;

  // Load up Chat Information
  ResponseString := await(MainForm.JSONRequest('IChatService.GetChatInformation',[]));
  if ResponseString <> '' then
  begin
    Here := Self;
    asm
      var data = JSON.parse(ResponseString);
      console.log(data);
      if (data.Models.length > 0) {

        Here.InitializeChat();

        // Use this to automatically add splitters
        var splittercheck = data.Models[0].replace('*** ','').substring(0,4);

        // Add all Models
        for (var i = 0; i < data.Models.length; i++) {
          if (data.Models[i].indexOf('*** ') == 0) {
            data.Models[i] = data.Models[i].replace('*** ','');
            ChatModelSelection.innerHTML = data.Models[i];
          }
          if (splittercheck !== data.Models[i].substring(0,4)) {
            splittercheck = data.Models[i].substring(0,4);
            ChatModelMenu.innerHTML += '<div class="dropdown-divider"></div>';
          }
          ChatModelMenu.innerHTML += '<div class="dropdown-item cursor-pointer ChatModelMenuItem">'+data.Models[i]+'</div>';
        }

        // If Model clicked, update button.  Also enable/disable image count
        ChatModelMenu.addEventListener('click', (e) => {
          if (e.target.classList.contains('ChatModelMenuItem')) {

            // Reset chat interface each time we change the model
            Here.InitializeChat();

            ChatModelSelection.innerHTML = e.target.innerHTML;
            if (e.target.innerHTML.indexOf('Image') == -1) {
              ChatCountSelection.setAttribute('disabled','');
            } else {
              ChatCountSelection.removeAttribute('disabled');
            }
          }
        });

        // If Image count clicked, update button
        ChatCountMenu.addEventListener('click', (e) => {
          if (e.target.classList.contains('ChatCountMenuItem')) {
            ChatCountSelection.innerHTML = "Images: "+e.target.innerHTML;
          }
        });
      }
      // No Models = No Chat
      else {
        btnChatSend.setAttribute('disabled');
        ChatCountSelection.setAttribute('disabled');
        ChatModelSelect.setSttribute('disabled');
      }

      // Chat Usage
      ChatUsage1.innerHTML = '<div class="d-flex">'+
                               '<div style="width:50px;" class="text-white">Requests</div>'+
                               '<div style="width:60px;" class="text-end text-info">'+data['ChatAI Usage'][1].requests+'</div>'+
                               '<div style="width:45px;" class="text-end"> / <small>'+data['ChatAI Usage'][1].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">'+data['ChatAI Usage'][0].requests+'</div>'+
                               '<div style="width:50px;" class="text-end"> / <small>'+data['ChatAI Usage'][0].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">'+data['ChatAI Usage'][2].requests+'</div>'+
                               '<div style="width:30px;" class="text-end"> / <small>'+data['ChatAI Usage'][2].period+'</small></div>'+
                             '</div>';
      ChatUsage2.innerHTML = '<div class="d-flex">'+
                               '<div style="width:50px;" class="text-white">Costs</div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+data['ChatAI Usage'][1].cost.toFixed(3)+'</div>'+
                               '<div style="width:45px;" class="text-end"> / <small>'  +data['ChatAI Usage'][1].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+data['ChatAI Usage'][0].cost.toFixed(3)+'</div>'+
                               '<div style="width:50px;" class="text-end"> / <small>'  +data['ChatAI Usage'][0].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+data['ChatAI Usage'][2].cost.toFixed(3)+'</div>'+
                               '<div style="width:30px;" class="text-end"> / <small>'  +data['ChatAI Usage'][2].period+'</small></div>'+
                             '</div>';
      ImageUsage1.innerHTML = '<div class="d-flex">'+
                               '<div style="width:50px;" class="text-white">Requests</div>'+
                               '<div style="width:60px;" class="text-end text-info">'+data['ImageAI Usage'][1].requests+'</div>'+
                               '<div style="width:45px;" class="text-end"> / <small>'+data['ImageAI Usage'][1].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">'+data['ImageAI Usage'][0].requests+'</div>'+
                               '<div style="width:50px;" class="text-end"> / <small>'+data['ImageAI Usage'][0].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">'+data['ImageAI Usage'][2].requests+'</div>'+
                               '<div style="width:30px;" class="text-end"> / <small>'+data['ImageAI Usage'][2].period+'</small></div>'+
                             '</div>';
      ImageUsage2.innerHTML = '<div class="d-flex">'+
                               '<div style="width:50px;" class="text-white">Costs</div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+data['ImageAI Usage'][1].cost.toFixed(3)+'</div>'+
                               '<div style="width:45px;" class="text-end"> / <small>'  +data['ImageAI Usage'][1].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+data['ImageAI Usage'][0].cost.toFixed(3)+'</div>'+
                               '<div style="width:50px;" class="text-end"> / <small>'  +data['ImageAI Usage'][0].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+data['ImageAI Usage'][2].cost.toFixed(3)+'</div>'+
                               '<div style="width:30px;" class="text-end"> / <small>'  +data['ImageAI Usage'][2].period+'</small></div>'+
                             '</div>';
      CombinedUsage1.innerHTML = '<div class="d-flex">'+
                               '<div style="width:50px;" class="text-white">Requests</div>'+
                               '<div style="width:60px;" class="text-end text-info">'+(data['ChatAI Usage'][1].requests+data['ImageAI Usage'][1].requests)+'</div>'+
                               '<div style="width:45px;" class="text-end"> / <small>' +data['ChatAI Usage'][1].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">'+(data['ChatAI Usage'][0].requests+data['ImageAI Usage'][0].requests)+'</div>'+
                               '<div style="width:50px;" class="text-end"> / <small>' +data['ChatAI Usage'][0].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">'+(data['ChatAI Usage'][2].requests+data['ImageAI Usage'][2].requests)+'</div>'+
                               '<div style="width:30px;" class="text-end"> / <small>' +data['ChatAI Usage'][2].period+'</small></div>'+
                             '</div>';
      CombinedUsage2.innerHTML = '<div class="d-flex">'+
                               '<div style="width:50px;" class="text-white">Costs</div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+(data['ChatAI Usage'][1].cost+data['ImageAI Usage'][1].cost).toFixed(3)+'</div>'+
                               '<div style="width:45px;" class="text-end"> / <small>'   +data['ChatAI Usage'][1].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+(data['ChatAI Usage'][0].cost+data['ImageAI Usage'][0].cost).toFixed(3)+'</div>'+
                               '<div style="width:50px;" class="text-end"> / <small>'   +data['ChatAI Usage'][0].period+'</small></div>'+
                               '<div style="width:60px;" class="text-end text-info">$ '+(data['ChatAI Usage'][2].cost+data['ImageAI Usage'][2].cost).toFixed(3)+'</div>'+
                               '<div style="width:30px;" class="text-end"> / <small>'   +data['ChatAI Usage'][2].period+'</small></div>'+
                             '</div>';

      // Recent Images
      for (var i = 0; i < data['ImageAI Recent'].length; i++) {
        ImageRecent.innerHTML += '<div class="cursor-pointer ViewableImage" title='+data['ImageAI Recent'][i].prompt+'>'+data['ImageAI Recent'][i].generated_image+'</div>';
      }
      ImageRecent.addEventListener('click', (e) => {
        if (e.target.parentElement.classList.contains('ViewableImage')) {
          e.stopImmediatePropagation();
          pas.UnitMain.MainForm.Viewer(e.target.parentElement.outerHTML);
        }
      });

      // Recent Chats
      var tabRecentChat = new Tabulator("#ChatRecent",{
        data: data['ChatAI Recent'],
        layout: "fitColumns",
        selectable: 1,
        height: "100%",
        headerVisible: false,
        columns: [
          { title: "Conversation", field: "conversation", formatter: "html" },
          { title: "Context", field: "context", visible: false },
          { title: "Response", field: "response", visible: false }
        ]
      });

      tabRecentChat.on("rowDblClick", function(e, row){
        var chat = '';
        var conv = row.getCell('conversation').getValue();
        var ctx = JSON.parse('['+row.getCell('context').getValue().replaceAll('\\"','')+']').slice(0,-1);
        var resp = row.getCell('response').getValue();

        chat = '<section class="w-100 h-100 position-absolute">'+
               '<div class="direct-chat direct-chat-warning border border-secondary bg-dark rounded m-1 pe-3 pb-3 w-100 h-100 position-absolute fs-6">'+
               '<div class="direct-chat-messages p-3 m-2 w-100 h-100 overflow-auto">';

        for (var i = 0; i < ctx.length; i++) {
          if (ctx[i].role == 'assistant') {
            chat += '<div class="direct-chat-msg">'+pas.UnitIcons.DMIcons.Lookup['Robot_Avatar']+'<div class="direct-chat-text">'+ctx[i].content+'</div></div>';
          } else {
            chat += '<div class="direct-chat-msg end">'+pas.UnitIcons.DMIcons.Lookup['User_Avatar']+'<div class="direct-chat-text">'+ctx[i].content+'</div></div>';
          }
        }

        chat += '<div class="direct-chat-msg end">'+pas.UnitIcons.DMIcons.Lookup['User_Avatar']+'<div class="direct-chat-text">'+conv+'</div></div>'+
                '<div class="direct-chat-msg">'+pas.UnitIcons.DMIcons.Lookup['Robot_Avatar']+'<div class="direct-chat-text">'+resp+'</div></div>'+
                '</div></div></section>';
        pas.UnitMain.MainForm.Viewer(chat);
      });

    end;
  end;


  asm
    window.document.dispatchEvent(new Event("DOMContentLoaded", {
      bubbles: true,
      cancelable: true
    }));
  end;

  (document.getElementById('divSubForm') as TJSHTMLElement).style.setProperty('opacity', '1','important');
  MainForm.LogAction('', False);

  WebTimer1.Enabled := True;
end;

procedure TChatStatisticsSubForm.WebFormResize(Sender: TObject);
begin
  // Chat Window adjustments
  asm
    ChatWindow.style.setProperty('height','auto');
    ChatWindow.style.setProperty('min-height','300px');
    ChatWindow.style.setProperty('max-height',(divSubForm.offsetHeight-321)+'px');
  end;
end;

procedure TChatStatisticsSubForm.WebTimer1Timer(Sender: TObject);
begin
  WebTimer1.Enabled := False;
  memoChat.SetFocus;
end;

end.