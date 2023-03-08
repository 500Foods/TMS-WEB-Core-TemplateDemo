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
    procedure AddMenuGroup(SideMenu: String; MenuGroup:String; Dashboard: String);
    procedure AddMenuItem(MenuGroup:String; MenuItem: String; Dashboard: String);
    [async] procedure MenuClicked(Dashboard: String; MenuGroup: String; MenuName: String; UserAction: Boolean);
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
      menuEntry.setAttribute('id',dash.replace('_','')+'_Dashboard_'+Dashboard.replace('_','')+'Sub');
      var menuLink = document.createElement('div');
      menuLink.innerHTML = icon[dash.replace('_','')+'_Menu']+'<p>'+dash+'</p>';

      // Highlight dashboard item if it is the current dashboard
      if (dash == Dashboard) { menuLink.className = 'pe-none nav-link active'; }
      else { menuLink.className = 'pe-none nav-link'; }

      // And menu item to menu tree
      menuTree.appendChild(menuEntry);
      menuEntry.appendChild(menuLink);

      // Add click event
      menuEntry.addEventListener('click', (menu) => {
        pas.UnitMenus.DMMenus.MenuClicked(
          menu.target.id.split('_')[0],
          menu.target.id.split('_')[1],
          menu.target.id.split('_')[2],
          true
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
    menuName.innerHTML = '<strong>'+icon[MenuGroup.replace('_','')+'_Menu']+'</strong><p class="pe-none"><strong>'+MenuGroup.replace('_',' ')+'</strong>'+icon["ArrowRight_Nav"]+'</p>';

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
    menuEntry.setAttribute('id',Dashboard.replace('_','')+'_'+MenuGroup.replace('_','')+'_'+MenuItem.replace('_',''));
    var menuLink = document.createElement('div');
    menuLink.innerHTML = icon[MenuItem.replace('_','')+'_Menu']+'<p>'+MenuItem.replace('_',' ')+'</p>';
    menuLink.className = 'pe-none nav-link';

    // And menu item to menu tree
    menuTree.appendChild(menuEntry);
    menuEntry.appendChild(menuLink);

    // Add click event
    menuEntry.addEventListener('click', (menu) => {
      pas.UnitMenus.DMMenus.MenuClicked(
        menu.target.id.split('_')[0],
        menu.target.id.split('_')[1],
        menu.target.id.split('_')[2],
        true
      );
    });
  end;
end;

procedure TDMMenus.MenuClicked(Dashboard: String; MenuGroup: String; MenuName: String; UserAction: Boolean);
begin
  // Load either a new dashboard or a subform on the current dashboard
  if MainForm.CurrentFormName = Dashboard+'Form' then
  begin
    // Load a SubForm (aka Page)
    if (UserAction) then MainForm.LogAction('Page Selected ['+Dashboard+'] ['+MenuGroup+'] ['+MenuName+']',true);
    MainForm.LoadSubForm(MenuName,DMIcons.Icon(MenuName+'_Menu'));
  end
  else
  begin
    // Load a Form (aka Dashboard)
    if (UserAction) then MainForm.LogAction('Dash Selected ['+Dashboard+'] ['+MenuGroup+'] ['+MenuName+']',true);
    MainForm.LoadForm(Dashboard+'Form',DMIcons.Icon(Dashboard+'_Menu'));
  end;
end;

end.
