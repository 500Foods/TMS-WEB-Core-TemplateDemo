unit UnitMenus;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules;

type
  TDMMenus = class(TWebDataModule)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddDashboards(SideMenu: String; Dashboard: String);
    [async] procedure MenuClicked(MenuForm: String; MenuType: String; MenuName: String);
    procedure AddMenuGroup(SideMenu: String; MenuGroup:String; Dashboard: String);
    procedure AddMenuItem(MenuGroup:String; MenuItem: String; Dashboard: String);
  end;

var
  DMMenus: TDMMenus;

implementation

uses UnitMain, UnitIcons;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDMMenus }

procedure TDMMenus.AddDashboards(SideMenu: String; Dashboard: String);
begin
  asm
    // Setup a link to the sidebar menu and to our icons
    var menu = document.getElementById(SideMenu);
    var icon = pas.UnitIcons.DMIcons.Lookup;
    var roles = JSON.parse('['+pas.UnitMain.MainForm.Roles+']');

    // Top-level of this menu block
    var menuTop = document.createElement('li');
    menuTop.className = 'nav-item menu-open';

    // Header for this menu block
    var menuName = document.createElement('div');
    menuName.className = 'nav-link active px-1 cursor-pointer';
    menuName.innerHTML = '<strong>'+icon["Dashboard_Nav"]+'</strong><p><strong>Dashboards</strong>'+icon["ArrowRight_Nav"]+'</p>';

    // Add to page
    menu.appendChild(menuTop);
    menuTop.appendChild(menuName);

    // Build tree
    var menuTree = document.createElement('ul');
    menuTree.className = 'nav nav-treeview';

    // Use roles to generate the list of dashboards
    var dashboards = new Set();
    for (var i = 0; i < roles.length; i++) {
      // Administrator Dashboards
      if (roles[i] == 1) {
        dashboards.add('Administrator');
        dashboards.add('People');
        dashboards.add('Labels');
      }
    }

    // For each element of the list, add a menu item
    dashboards.forEach(dash => {

      // Create menu item
      var menuEntry = document.createElement('li');
      menuEntry.className = 'nav-item cursor-pointer';
      var menuLink = document.createElement('div');
      menuLink.innerHTML = icon[dash.replace('_','')+'_Menu']+'<p>'+dash+'</p>';
      menuLink.setAttribute('id',Dashboard.replace('_','')+'_Dashboard_'+dash.replace('_',''));

      // Highlight dashboard item if it is the current dashboard
      if (dash == Dashboard) { menuLink.className = 'nav-link active'; }
      else { menuLink.className = 'nav-link'; }

      // And menu item to menu tree
      menuTree.appendChild(menuEntry);
      menuEntry.appendChild(menuLink);

      // Add click event
      menuLink.addEventListener('click', (menu) => {
        pas.UnitMenus.DMMenus.MenuClicked(
          menu.target.id.split('_')[0],
          menu.target.id.split('_')[1],
          menu.target.id.split('_')[2]
        );
      });
    });

    // Add the menu tree to the menu
    menuTop.appendChild(menuTree);
  end;

end;

procedure TDMMenus.AddMenuGroup(SideMenu: String; MenuGroup:String; Dashboard: String);
begin
  asm
    // Setup a link to the sidebar menu and to our icons
    var menu = document.getElementById(SideMenu);
    var icon = pas.UnitIcons.DMIcons.Lookup;

    // Top-level of this menu block
    var menuTop = document.createElement('li');
    menuTop.className = 'nav-item menu-close';

    // Header for this menu block
    var menuName = document.createElement('div');
    menuName.className = 'nav-link active px-1 cursor-pointer';
    menuName.innerHTML = '<strong>'+icon[MenuGroup.replace('_','')+'_Menu']+'</strong><p><strong>'+MenuGroup.replace('_',' ')+'</strong>'+icon["ArrowRight_Nav"]+'</p>';

    // Add to page
    menu.appendChild(menuTop);
    menuTop.appendChild(menuName);

    // Build tree
    var menuTree = document.createElement('ul');
    menuTree.className = 'nav nav-treeview';
    menuTree.id = 'Menu_'+Dashboard.replace('_','')+'_'+MenuGroup.replace('_','');
    menuTop.appendChild(menuTree);
  end;

end;

procedure TDMMenus.AddMenuItem(MenuGroup, MenuItem, Dashboard: String);
begin
  asm
    var menuTree = document.getElementById('Menu_'+Dashboard.replace('_','')+'_'+MenuGroup.replace('_',''));
    var icon = pas.UnitIcons.DMIcons.Lookup;

    // Create menu item
    var menuEntry = document.createElement('li');
    menuEntry.className = 'nav-item cursor-pointer';
    var menuLink = document.createElement('div');
    menuLink.innerHTML = icon[MenuItem.replace('_','')+'_Menu']+'<p>'+MenuItem.replace('_',' ')+'</p>';
    menuLink.setAttribute('id',Dashboard.replace('_','')+'_'+MenuGroup.replace('_','')+'_'+MenuItem.replace('_',''));
    menuLink.className = 'nav-link';

    // And menu item to menu tree
    menuTree.appendChild(menuEntry);
    menuEntry.appendChild(menuLink);

    // Add click event
    menuEntry.addEventListener('click', (menu) => {
      pas.UnitMain.MainForm.CurrentForm.MenuClicked(
        menu.target.id.split('_')[0],
        menu.target.id.split('_')[1],
        menu.target.id.split('_')[2]
      );
    });
  end;
end;

procedure TDMMenus.MenuClicked(MenuForm: String; MenuType: String; MenuName: String);
var
  FormIcon: String;
begin
  MainForm.LogAction('Menu Clicked ['+MenuForm+'] ['+MenuType+'] ['+MenuName+']',true);

  asm
    FormIcon = icon[MenuName+'_Menu'];
  end;

  // Hide either the current subform or the main form
  if MainForm.CurrentFormName = MenuName then
  begin
    asm
      document.getElementById('divSubForm').style.setProperty('opacity','0')
      pas.UnitMain.MainForm.CurrentForm.MenuClicked(MenuForm, MenuType, MenuName);
    end
  end
  else
  begin
    MainForm.divHost.ElementHandle.style.setProperty('opacity','0');
    asm await sleep(500); end;
    MainForm.LoadForm(MenuName,FormIcon);
  end;
end;

end.
