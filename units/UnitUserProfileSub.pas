unit UnitUserProfileSub;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls, WEBLib.JSON,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls,
  WEBLib.WebCtrls;

type
  TUserProfileSubForm = class(TWebForm)
    labelDashboard: TWebLabel;
    bcDashboards: TWebLabel;
    bcDashboard: TWebLabel;
    bcCurrent: TWebLabel;
    iconBirthday: TWebLabel;
    labelBirthday: TWebLabel;
    iconAnniversary: TWebLabel;
    labelAnniversary: TWebLabel;
    iconEMail: TWebLabel;
    labelEMail: TWebLabel;
    iconPhone: TWebLabel;
    labelPhone: TWebLabel;
    iconLastLogin: TWebLabel;
    labelLastLogin: TWebLabel;
    labelRecentLogins: TWebLabel;
    iconRecentLogins: TWebLabel;
    imageProfile: TWebHTMLDiv;
    titleImage: TWebLabel;
    tableContacts: TWebHTMLDiv;
    titleContacts: TWebLabel;
    titleHistory: TWebLabel;
    tableHistory: TWebHTMLDiv;
    procedure bcDashboardClick(Sender: TObject);
    [async] procedure WebFormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserProfileSubForm: TUserProfileSubForm;

implementation

uses
  UnitMain, UnitIcons, UnitMenus;

{$R *.dfm}

procedure TUserProfileSubForm.WebFormCreate(Sender: TObject);
var
  ResponseString: String;

begin
  labelDashboard.HTML := DMIcons.Icon('Profile_Menu')+'User Profile';
  bcDashboards.HTML := DMICons.Icon('Dashboard_Menu')+'Dashboards';
  bcDashboard.HTML := mainForm.CurrentFormIcon+MainForm.CurrentFormName;
  bcCurrent.hTML := DMIcons.Icon('Profile_Menu')+'User Profile';

  ResponseString := await(MainForm.JSONRequest('IPersonService.profile',[]));
  if ResponseString <> '' then
  begin
    asm
      var icon = pas.UnitIcons.DMIcons.Lookup;
      var data = JSON.parse(ResponseString);

//      console.log(data);

      iconBirthday.innerHTML = icon['Birthday'];
      labelBirthday.innerHTML =  luxon.DateTime.fromISO(data['Profile'][0]['birthdate'].split(' ')[0]).toFormat('yyyy-MMM-dd');

      iconAnniversary.innerHTML = icon['Anniversary'];
      labelAnniversary.innerHTML = luxon.DateTime.fromISO(data['Role'][0]['valid_after'].split(' ')[0]).toFormat('yyyy-MMM-dd');

      var email = -1;
      var phone = -1;
      for (var i = 0; i < data['Contact'].length; i++) {
        if (data['Contact'][i]['list_contact'] == 1) {
          if (email == -1) {
            email = i;
          }
        }
        else if (data['Contact'][i]['list_contact'] == 3) {
          if (phone == -1) {
            phone = i;
          }
        }
      }
      iconEMail.innerHTML = icon['EMail'];
      if (email !== -1) {
        labelEMail.innerHTML = '<a title="'+data['Contact'][email]['value']+'" href=mailto:"'+data['Contact'][email]['value']+'">'+data['Contact'][email]['value']+'</a>';
      }
      iconPhone.innerHTML = icon['Telephone'];
      if (phone !== -1) {
        labelPhone.innerHTML = '<a title="'+data['Contact'][phone]['value']+'" href="tel:'+data['Contact'][phone]['value']+'">'+data['Contact'][phone]['value']+'</a>';
      }

      var lastlogin = luxon.DateTime.fromISO(data['RecentLogins'][0]['logged_in'].split(' ').join('T'),{zone:"utc"}).setZone("system").toFormat('yyyy-MMM-dd HH:mm');
      iconLastLogin.innerHTML = icon['Login'];
      labelLastLogin.innerHTML = '<span title="'+lastlogin+'">'+lastlogin+'</span>';

      iconRecentLogins.innerHTML = icon['Clock'];
      labelRecentLogins.innerHTML = data['RecentLogins'].length+' <small class="text-secondary me-3"> 7d </small> '+data['Logins'][0]['logins']+' <small class="text-secondary"> All </small>';

      titleImage.innerHTML = icon['Photo']+'<span class="ms-2">Profile Photo</span>';
      imageProfile.innerHTML = data['Photo'];
      imageProfile.firstElementChild.style.setProperty('width','100%');
      imageProfile.firstElementChild.style.setProperty('border-bottom-left-radius','var(--custom-rounding)');
      imageProfile.firstElementChild.style.setProperty('border-bottom-right-radius','var(--custom-rounding)');

      titleContacts.innerHTML = icon['AddressBook']+'<span class="ms-2">Contacts for '+data['Profile'][0]['first_name']+' '+data['Profile'][0]['last_name']+'</span>';
      var tablerows = '';
      for (var i = 0; i < data['Contact'].length; i++) {
        tablerows += '<tr>'
        var contact_icon = data['Contact'][i]['contact_attributes'];
        if (contact_icon.indexOf(':') == 0) {
          contact_icon = icon[contact_icon.replace(':','')];
        }
        tablerows += '<td>'+contact_icon+'</td>';
        tablerows += '<td>'+data['Contact'][i]['contact_type']+'</td>';
        tablerows += '<td>'+data['Contact'][i]['value']+'</td>';
        tablerows += '</tr>'
      }
      tableContacts.innerHTML = tablerows;

      titleHistory.innerHTML = icon['Clock']+'<span class="ms-2">Recent Login History</span>';
      tableHistory.style.setProperty('max-height','525px');
      tableHistory.style.setProperty('border-bottom-left-radius','var(--custom-rounding)');
      tableHistory.style.setProperty('border-bottom-right-radius','var(--custom-rounding)');
      var tabHistory = new Tabulator("#tableHistory",{
        data: data['RecentLogins'],
        layout: "fitColumns",
        selectable: 1,
        columns: [
          { title: "Logged In", field: "logged_in", formatter: function(cell, formatterParams, onRendered) {
            return luxon.DateTime.fromISO(cell.getValue().split(' ').join('T'),{zone:"utc"}).setZone("system").toFormat('yyyy-MMM-dd HH:mm:ss');
          }},
          { title: "IP Address", field: "ip_address" }
        ]
      });

    end;
  end;

  asm
//    menuSidebar.replaceWith(menuSidebar.cloneNode(true));
//    pas.UnitMain.MainForm.CurrentForm.CreateMenu();
    window.document.dispatchEvent(new Event("DOMContentLoaded", {
      bubbles: true,
      cancelable: true
    }));
  end;

  (document.getElementById('divSubForm') as TJSHTMLElement).style.setProperty('opacity', '1','important');
  MainForm.LogAction('', False);
end;

procedure TUserProfileSubForm.bcDashboardClick(Sender: TObject);
begin
  DMMenus.MenuClicked(StringReplace(MainForm.CurrentFormName,'Form','',[]),'Dashboard',StringReplace(MainForm.CurrentFormName,'Form','',[])+'Sub', True)
end;

end.