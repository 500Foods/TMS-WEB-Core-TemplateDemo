/*!
 * AdminLTE v4.0.0-alpha1 (https://adminlte.io)
 * Copyright 2014-2023 Colorlib <https://colorlib.com>
 * Licensed under MIT (https://github.com/ColorlibHQ/AdminLTE/blob/master/LICENSE)
 */
(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
    typeof define === 'function' && define.amd ? define(['exports'], factory) :
    (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.adminlte = {}));
})(this, (function (exports) { 'use strict';

    const domReady = (callBack) => {
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', callBack);
        }
        else {
            callBack();
        }
    };
    /* SLIDE UP */
    const slideUp = (target, duration = 500) => {
        target.style.transitionProperty = 'height, margin, padding';
        target.style.transitionDuration = `${duration}ms`;
        target.style.boxSizing = 'border-box';
        target.style.height = `${target.offsetHeight}px`;
        target.style.overflow = 'hidden';
        window.setTimeout(() => {
            target.style.height = '0';
            target.style.paddingTop = '0';
            target.style.paddingBottom = '0';
            target.style.marginTop = '0';
            target.style.marginBottom = '0';
        }, 1);
        window.setTimeout(() => {
            target.style.display = 'none';
            target.style.removeProperty('height');
            target.style.removeProperty('padding-top');
            target.style.removeProperty('padding-bottom');
            target.style.removeProperty('margin-top');
            target.style.removeProperty('margin-bottom');
            target.style.removeProperty('overflow');
            target.style.removeProperty('transition-duration');
            target.style.removeProperty('transition-property');
        }, duration);
    };
    /* SLIDE DOWN */
    const slideDown = (target, duration = 500) => {
        target.style.removeProperty('display');
        let { display } = window.getComputedStyle(target);
        if (display === 'none') {
            display = 'block';
        }
        target.style.display = display;
        const height = target.offsetHeight;
        target.style.overflow = 'hidden';
        target.style.height = '0';
        target.style.paddingTop = '0';
        target.style.paddingBottom = '0';
        target.style.marginTop = '0';
        target.style.marginBottom = '0';
        window.setTimeout(() => {
            target.style.boxSizing = 'border-box';
            target.style.transitionProperty = 'height, margin, padding';
            target.style.transitionDuration = `${duration}ms`;
            target.style.height = `${height}px`;
            target.style.removeProperty('padding-top');
            target.style.removeProperty('padding-bottom');
            target.style.removeProperty('margin-top');
            target.style.removeProperty('margin-bottom');
        }, 1);
        window.setTimeout(() => {
            target.style.removeProperty('height');
            target.style.removeProperty('overflow');
            target.style.removeProperty('transition-duration');
            target.style.removeProperty('transition-property');
        }, duration);
    };

    /**
     * --------------------------------------------
     * AdminLTE layout.ts
     * License MIT
     * --------------------------------------------
     */
    /**
     * ------------------------------------------------------------------------
     * Constants
     * ------------------------------------------------------------------------
     */
    const CLASS_NAME_HOLD_TRANSITIONS = 'hold-transition';
    /**
     * Class Definition
     * ====================================================
     */
    class Layout {
        constructor(element, config) {
            this._element = element;
            this._config = config;
        }
        holdTransition() {
            let resizeTimer;
            window.addEventListener('resize', () => {
                document.body.classList.add(CLASS_NAME_HOLD_TRANSITIONS);
                clearTimeout(resizeTimer);
                resizeTimer = setTimeout(() => {
                    document.body.classList.remove(CLASS_NAME_HOLD_TRANSITIONS);
                }, 400);
            });
        }
    }
    domReady(() => {
        const data = new Layout(document.body, undefined);
        data.holdTransition();
    });

    /**
     * --------------------------------------------
     * AdminLTE push-menu.ts
     * License MIT
     * --------------------------------------------
     */
    /**
     * ------------------------------------------------------------------------
     * Constants
     * ------------------------------------------------------------------------
     */
    const DATA_KEY$3 = 'lte.push-menu';
    const EVENT_KEY$3 = `.${DATA_KEY$3}`;
    const EVENT_OPEN = `open${EVENT_KEY$3}`;
    const EVENT_COLLAPSE = `collapse${EVENT_KEY$3}`;
    const EVENT_CLOSE = `close${EVENT_KEY$3}`;
    const CLASS_NAME_SIDEBAR_MINI = 'sidebar-mini';
    const CLASS_NAME_SIDEBAR_COLLAPSE = 'sidebar-collapse';
    const CLASS_NAME_SIDEBAR_CLOSE = 'sidebar-close';
    const CLASS_NAME_SIDEBAR_OPEN = 'sidebar-open';
    const CLASS_NAME_SIDEBAR_IS_HOVER = 'sidebar-is-hover';
    const CLASS_NAME_MENU_OPEN$1 = 'menu-open';
    const CLASS_NAME_LAYOUT_MOBILE = 'layout-mobile';
    const SELECTOR_SIDEBAR_WRAPPER = '.sidebar-wrapper';
    const SELECTOR_SIDEBAR_MENU = '.sidebar-menu';
    const SELECTOR_NAV_ITEM$1 = '.nav-item';
    const SELECTOR_NAV_TREEVIEW = '.nav-treeview';
    const SELECTOR_MINI_TOGGLE = '[data-lte-toggle="sidebar-mini"]';
    const SELECTOR_FULL_TOGGLE = '[data-lte-toggle="sidebar-full"]';
    const SELECTOR_LAYOUT_MOBILE = `.${CLASS_NAME_LAYOUT_MOBILE}`;
    const SELECTOR_APP_MAIN = '.app-main';
    const Defaults = {
        onLayoutMobile: 992
    };
    /**
     * Class Definition
     * ====================================================
     */
    class PushMenu {
        constructor(element, config) {
            this._element = element;
            const bodyElement = document.body;
            this._bodyClass = bodyElement.classList;
            this._config = config;
        }
        // TODO
        menusClose() {
            const navTreeview = document.querySelectorAll(SELECTOR_NAV_TREEVIEW);
            for (const navTree of navTreeview) {
                navTree.style.removeProperty('display');
                navTree.style.removeProperty('height');
            }
            const navSidebar = document.querySelector(SELECTOR_SIDEBAR_MENU);
            const navItem = navSidebar === null || navSidebar === void 0 ? void 0 : navSidebar.querySelectorAll(SELECTOR_NAV_ITEM$1);
            if (navItem) {
                for (const navI of navItem) {
                    navI.classList.remove(CLASS_NAME_MENU_OPEN$1);
                }
            }
        }
        expand() {
            var _a;
            const event = new Event(EVENT_OPEN);
            this._bodyClass.remove(CLASS_NAME_SIDEBAR_CLOSE);
            this._bodyClass.remove(CLASS_NAME_SIDEBAR_COLLAPSE);
            this._bodyClass.add(CLASS_NAME_SIDEBAR_OPEN);
            (_a = this._element) === null || _a === void 0 ? void 0 : _a.dispatchEvent(event);
        }
        collapse() {
            var _a;
            const event = new Event(EVENT_COLLAPSE);
            this._bodyClass.remove(CLASS_NAME_SIDEBAR_OPEN);
            this._bodyClass.remove(CLASS_NAME_SIDEBAR_CLOSE);
            this._bodyClass.add(CLASS_NAME_SIDEBAR_COLLAPSE);
            (_a = this._element) === null || _a === void 0 ? void 0 : _a.dispatchEvent(event);
        }
        close() {
            var _a;
            const event = new Event(EVENT_CLOSE);
            this._bodyClass.remove(CLASS_NAME_SIDEBAR_OPEN);
            this._bodyClass.remove(CLASS_NAME_SIDEBAR_COLLAPSE);
            this._bodyClass.add(CLASS_NAME_SIDEBAR_CLOSE);
            (_a = this._element) === null || _a === void 0 ? void 0 : _a.dispatchEvent(event);
        }
        sidebarHover() {
            const selSidebar = document.querySelector(SELECTOR_SIDEBAR_WRAPPER);
            if (selSidebar) {
                selSidebar.addEventListener('mouseover', () => {
                    this._bodyClass.add(CLASS_NAME_SIDEBAR_IS_HOVER);
                });
                selSidebar.addEventListener('mouseout', () => {
                    this._bodyClass.remove(CLASS_NAME_SIDEBAR_IS_HOVER);
                });
            }
        }
        addSidebarBreakPoint() {
            const bodyClass = document.body.classList;
            const widthOutput = window.innerWidth;
            if (widthOutput < Defaults.onLayoutMobile) {
                bodyClass.add(CLASS_NAME_LAYOUT_MOBILE);
                this.close();
            }
            if (widthOutput >= Defaults.onLayoutMobile) {
                bodyClass.remove(CLASS_NAME_LAYOUT_MOBILE);
                if (!bodyClass.contains(CLASS_NAME_SIDEBAR_MINI)) {
                    this.expand();
                }
                if (bodyClass.contains(CLASS_NAME_SIDEBAR_MINI)) {
                    this.collapse();
                }
            }
        }
        removeOverlaySidebar() {
            const bodyClass = document.body.classList;
            if (bodyClass.contains(CLASS_NAME_LAYOUT_MOBILE)) {
                bodyClass.remove(CLASS_NAME_SIDEBAR_OPEN);
                bodyClass.remove(CLASS_NAME_SIDEBAR_COLLAPSE);
                bodyClass.add(CLASS_NAME_SIDEBAR_CLOSE);
            }
        }
        toggleFull() {
            if (this._bodyClass.contains(CLASS_NAME_SIDEBAR_CLOSE)) {
                this.expand();
            }
            else {
                this.close();
            }
        }
        toggleMini() {
            if (this._bodyClass.contains(CLASS_NAME_SIDEBAR_COLLAPSE)) {
                this.expand();
            }
            else {
                this.collapse();
            }
        }
        init() {
            this.addSidebarBreakPoint();
            this.sidebarHover();
            const targetLayoutMobile = document.querySelector(SELECTOR_LAYOUT_MOBILE);
            const targetContentWrapper = targetLayoutMobile === null || targetLayoutMobile === void 0 ? void 0 : targetLayoutMobile.querySelector(SELECTOR_APP_MAIN);
            if (targetContentWrapper) {
                targetContentWrapper.addEventListener('touchstart', this.removeOverlaySidebar);
                targetContentWrapper.addEventListener('click', this.removeOverlaySidebar);
            }
        }
    }
    /**
     * ------------------------------------------------------------------------
     * Data Api implementation
     * ------------------------------------------------------------------------
     */
    domReady(() => {
        const data = new PushMenu(undefined, undefined);
        data.init();
        window.addEventListener('resize', () => {
            data.init();
        });
        const fullBtn = document.querySelectorAll(SELECTOR_FULL_TOGGLE);
        for (const btn of fullBtn) {
            btn.addEventListener('click', event => {
                event.preventDefault();
                let button = event.currentTarget;
                if ((button === null || button === void 0 ? void 0 : button.dataset.lteToggle) !== 'sidebar-full') {
                    button = button === null || button === void 0 ? void 0 : button.closest(SELECTOR_FULL_TOGGLE);
                }
                if (button) {
                    const data = new PushMenu(button, undefined);
                    data.toggleFull();
                }
            });
        }
        const miniBtn = document.querySelectorAll(SELECTOR_MINI_TOGGLE);
        for (const btn of miniBtn) {
            btn.addEventListener('click', event => {
                event.preventDefault();
                let button = event.currentTarget;
                if ((button === null || button === void 0 ? void 0 : button.dataset.lteToggle) !== 'sidebar-mini') {
                    button = button === null || button === void 0 ? void 0 : button.closest(SELECTOR_FULL_TOGGLE);
                }
                if (button) {
                    const data = new PushMenu(button, undefined);
                    data.toggleMini();
                }
            });
        }
    });

    /**
     * --------------------------------------------
     * AdminLTE treeview.ts
     * License MIT
     * --------------------------------------------
     */
    /**
     * ------------------------------------------------------------------------
     * Constants
     * ------------------------------------------------------------------------
     */
    // const NAME = 'Treeview'
    const DATA_KEY$2 = 'lte.treeview';
    const EVENT_KEY$2 = `.${DATA_KEY$2}`;
    const EVENT_EXPANDED$2 = `expanded${EVENT_KEY$2}`;
    const EVENT_COLLAPSED$2 = `collapsed${EVENT_KEY$2}`;
    // const EVENT_LOAD_DATA_API = `load${EVENT_KEY}`
    const CLASS_NAME_MENU_OPEN = 'menu-open';
    const SELECTOR_NAV_ITEM = '.nav-item';
    const SELECTOR_TREEVIEW_MENU = '.nav-treeview';
    const SELECTOR_DATA_TOGGLE$1 = '[data-lte-toggle="treeview"]';
    const Default$1 = {
        animationSpeed: 300
    };
    /**
     * Class Definition
     * ====================================================
     */
    class Treeview {
        constructor(element, config) {
            var _a, _b;
            this._element = element;
            this._config = { ...Default$1, ...config };
            this._navItem = (_a = this._element) === null || _a === void 0 ? void 0 : _a.closest(SELECTOR_NAV_ITEM);
            this._childNavItem = (_b = this._navItem) === null || _b === void 0 ? void 0 : _b.querySelector(SELECTOR_TREEVIEW_MENU);
        }
        open() {
            var _a;
            const event = new Event(EVENT_EXPANDED$2);
            if (this._navItem) {
                this._navItem.classList.add(CLASS_NAME_MENU_OPEN);
            }
            if (this._childNavItem) {
                slideDown(this._childNavItem, this._config.animationSpeed);
            }
            (_a = this._element) === null || _a === void 0 ? void 0 : _a.dispatchEvent(event);
        }
        close() {
            var _a;
            const event = new Event(EVENT_COLLAPSED$2);
            window.setTimeout(() => {
                if (this._navItem) {
                    this._navItem.classList.remove(CLASS_NAME_MENU_OPEN);
                }
            }, this._config.animationSpeed);
            if (this._childNavItem) {
                slideUp(this._childNavItem, this._config.animationSpeed);
            }
            (_a = this._element) === null || _a === void 0 ? void 0 : _a.dispatchEvent(event);
        }
        toggle() {
            var _a;
            if ((_a = this._navItem) === null || _a === void 0 ? void 0 : _a.classList.contains(CLASS_NAME_MENU_OPEN)) {
                this.close();
            }
            else {
                this.open();
            }
        }
    }
    /**
     * ------------------------------------------------------------------------
     * Data Api implementation
     * ------------------------------------------------------------------------
     */
    domReady(() => {
        const button = document.querySelectorAll(SELECTOR_DATA_TOGGLE$1);
        for (const btn of button) {
            btn.addEventListener('click', event => {
                const treeviewMenu = event.target;
                const data = new Treeview(treeviewMenu, Default$1);
                data.toggle();
            });
        }
    });

    /**
     * --------------------------------------------
     * AdminLTE direct-chat.ts
     * License MIT
     * --------------------------------------------
     */
    /**
     * Constants
     * ====================================================
     */
    const DATA_KEY$1 = 'lte.direct-chat';
    const EVENT_KEY$1 = `.${DATA_KEY$1}`;
    const EVENT_EXPANDED$1 = `expanded${EVENT_KEY$1}`;
    const EVENT_COLLAPSED$1 = `collapsed${EVENT_KEY$1}`;
    const SELECTOR_DATA_TOGGLE = '[data-lte-toggle="chat-pane"]';
    const SELECTOR_DIRECT_CHAT = '.direct-chat';
    const CLASS_NAME_DIRECT_CHAT_OPEN = 'direct-chat-contacts-open';
    /**
     * Class Definition
     * ====================================================
     */
    class DirectChat {
        constructor(element, config) {
            this._element = element;
            this._config = config;
        }
        toggle() {
            var _a, _b, _c, _d, _e;
            if ((_a = this._element) === null || _a === void 0 ? void 0 : _a.classList.contains(CLASS_NAME_DIRECT_CHAT_OPEN)) {
                const event = new Event(EVENT_COLLAPSED$1);
                (_b = this._element) === null || _b === void 0 ? void 0 : _b.classList.remove(CLASS_NAME_DIRECT_CHAT_OPEN);
                (_c = this._element) === null || _c === void 0 ? void 0 : _c.dispatchEvent(event);
            }
            else {
                const event = new Event(EVENT_EXPANDED$1);
                (_d = this._element) === null || _d === void 0 ? void 0 : _d.classList.add(CLASS_NAME_DIRECT_CHAT_OPEN);
                (_e = this._element) === null || _e === void 0 ? void 0 : _e.dispatchEvent(event);
            }
        }
    }
    /**
     *
     * Data Api implementation
     * ====================================================
     */
    domReady(() => {
        const button = document.querySelectorAll(SELECTOR_DATA_TOGGLE);
        for (const btn of button) {
            btn.addEventListener('click', event => {
                event.preventDefault();
                const target = event.target;
                const chatPane = target === null || target === void 0 ? void 0 : target.closest(SELECTOR_DIRECT_CHAT);
                const data = new DirectChat(chatPane, undefined);
                data.toggle();
            });
        }
    });

    /**
     * --------------------------------------------
     * AdminLTE card-widget.ts
     * License MIT
     * --------------------------------------------
     */
    /**
     * Constants
     * ====================================================
     */
    const DATA_KEY = 'lte.card-widget';
    const EVENT_KEY = `.${DATA_KEY}`;
    const EVENT_COLLAPSED = `collapsed${EVENT_KEY}`;
    const EVENT_EXPANDED = `expanded${EVENT_KEY}`;
    const EVENT_REMOVE = `remove${EVENT_KEY}`;
    const EVENT_MAXIMIZED = `maximized${EVENT_KEY}`;
    const EVENT_MINIMIZED = `minimized${EVENT_KEY}`;
    const CLASS_NAME_CARD = 'card';
    const CLASS_NAME_COLLAPSED = 'collapsed-card';
    const CLASS_NAME_COLLAPSING = 'collapsing-card';
    const CLASS_NAME_EXPANDING = 'expanding-card';
    const CLASS_NAME_WAS_COLLAPSED = 'was-collapsed';
    const CLASS_NAME_MAXIMIZED = 'maximized-card';
    const SELECTOR_DATA_REMOVE = '[data-lte-dismiss="card-remove"]';
    const SELECTOR_DATA_COLLAPSE = '[data-lte-toggle="card-collapse"]';
    const SELECTOR_DATA_MAXIMIZE = '[data-lte-toggle="card-maximize"]';
    const SELECTOR_CARD = `.${CLASS_NAME_CARD}`;
    const SELECTOR_CARD_HEADER = '.card-header';
    const SELECTOR_CARD_BODY = '.card-body';
    const SELECTOR_CARD_FOOTER = '.card-footer';
    const Default = {
        animationSpeed: 500,
        collapseTrigger: SELECTOR_DATA_COLLAPSE,
        removeTrigger: SELECTOR_DATA_REMOVE,
        maximizeTrigger: SELECTOR_DATA_MAXIMIZE,
        collapseIcon: 'fa-minus',
        expandIcon: 'fa-plus',
        maximizeIcon: 'fa-expand',
        minimizeIcon: 'fa-compress'
    };
    class CardWidget {
        constructor(element, config) {
            this._element = element;
            this._parent = element.closest(SELECTOR_CARD);
            if (element.classList.contains(CLASS_NAME_CARD)) {
                this._parent = element;
            }
            this._config = { ...Default, ...config };
        }
        collapse() {
            var _a, _b, _c;
            const event = new Event(EVENT_COLLAPSED);
            if (this._parent) {
                this._parent.classList.add(CLASS_NAME_COLLAPSING);
                const elm = (_a = this._parent) === null || _a === void 0 ? void 0 : _a.querySelectorAll(`${SELECTOR_CARD_BODY}, ${SELECTOR_CARD_FOOTER}`);
                if (elm !== undefined) {
                    for (const el of elm) {
                        if (el instanceof HTMLElement) {
                            slideUp(el, this._config.animationSpeed);
                        }
                    }
                }
                setTimeout(() => {
                    if (this._parent) {
                        this._parent.classList.add(CLASS_NAME_COLLAPSED);
                        this._parent.classList.remove(CLASS_NAME_COLLAPSING);
                    }
                }, this._config.animationSpeed);
            }
            const icon = (_b = this._parent) === null || _b === void 0 ? void 0 : _b.querySelector(`${SELECTOR_CARD_HEADER} ${this._config.collapseTrigger} .${this._config.collapseIcon}`);
            if (icon) {
                icon.classList.remove(this._config.collapseIcon);
                icon.classList.add(this._config.expandIcon);
            }
            (_c = this._element) === null || _c === void 0 ? void 0 : _c.dispatchEvent(event);
        }
        expand() {
            var _a, _b, _c;
            const event = new Event(EVENT_EXPANDED);
            if (this._parent) {
                this._parent.classList.add(CLASS_NAME_EXPANDING);
                const elm = (_a = this._parent) === null || _a === void 0 ? void 0 : _a.querySelectorAll(`${SELECTOR_CARD_BODY}, ${SELECTOR_CARD_FOOTER}`);
                if (elm !== undefined) {
                    for (const el of elm) {
                        if (el instanceof HTMLElement) {
                            slideDown(el, this._config.animationSpeed);
                        }
                    }
                }
                setTimeout(() => {
                    if (this._parent) {
                        this._parent.classList.remove(CLASS_NAME_COLLAPSED);
                        this._parent.classList.remove(CLASS_NAME_EXPANDING);
                    }
                }, this._config.animationSpeed);
            }
            const icon = (_b = this._parent) === null || _b === void 0 ? void 0 : _b.querySelector(`${SELECTOR_CARD_HEADER} ${this._config.collapseTrigger} .${this._config.expandIcon}`);
            if (icon) {
                icon.classList.add(this._config.collapseIcon);
                icon.classList.remove(this._config.expandIcon);
            }
            (_c = this._element) === null || _c === void 0 ? void 0 : _c.dispatchEvent(event);
        }
        remove() {
            var _a;
            const event = new Event(EVENT_REMOVE);
            if (this._parent) {
                slideUp(this._parent, this._config.animationSpeed);
            }
            (_a = this._element) === null || _a === void 0 ? void 0 : _a.dispatchEvent(event);
        }
        toggle() {
            var _a;
            if ((_a = this._parent) === null || _a === void 0 ? void 0 : _a.classList.contains(CLASS_NAME_COLLAPSED)) {
                this.expand();
                return;
            }
            this.collapse();
        }
        maximize() {
            var _a;
            const event = new Event(EVENT_MAXIMIZED);
            if (this._parent) {
                const maxElm = this._parent.querySelector(`${this._config.maximizeTrigger} .${this._config.maximizeIcon}`);
                if (maxElm) {
                    maxElm.classList.add(this._config.minimizeIcon);
                    maxElm.classList.remove(this._config.maximizeIcon);
                }
                this._parent.style.height = `${this._parent.scrollHeight}px`;
                this._parent.style.width = `${this._parent.scrollWidth}px`;
                this._parent.style.transition = 'all .15s';
                setTimeout(() => {
                    const htmlTag = document.querySelector('html');
                    if (htmlTag) {
                        htmlTag.classList.add(CLASS_NAME_MAXIMIZED);
                    }
                    if (this._parent) {
                        this._parent.classList.add(CLASS_NAME_MAXIMIZED);
                        if (this._parent.classList.contains(CLASS_NAME_COLLAPSED)) {
                            this._parent.classList.add(CLASS_NAME_WAS_COLLAPSED);
                        }
                    }
                }, 150);
            }
            (_a = this._element) === null || _a === void 0 ? void 0 : _a.dispatchEvent(event);
        }
        minimize() {
            var _a;
            const event = new Event(EVENT_MINIMIZED);
            if (this._parent) {
                const minElm = this._parent.querySelector(`${this._config.maximizeTrigger} .${this._config.minimizeIcon}`);
                if (minElm) {
                    minElm.classList.add(this._config.maximizeIcon);
                    minElm.classList.remove(this._config.minimizeIcon);
                }
                this._parent.style.cssText = `height: ${this._parent.style.height} !important; width: ${this._parent.style.width} !important; transition: all .15s;`;
                setTimeout(() => {
                    var _a;
                    const htmlTag = document.querySelector('html');
                    if (htmlTag) {
                        htmlTag.classList.remove(CLASS_NAME_MAXIMIZED);
                    }
                    if (this._parent) {
                        this._parent.classList.remove(CLASS_NAME_MAXIMIZED);
                        if ((_a = this._parent) === null || _a === void 0 ? void 0 : _a.classList.contains(CLASS_NAME_WAS_COLLAPSED)) {
                            this._parent.classList.remove(CLASS_NAME_WAS_COLLAPSED);
                        }
                    }
                }, 10);
            }
            (_a = this._element) === null || _a === void 0 ? void 0 : _a.dispatchEvent(event);
        }
        toggleMaximize() {
            var _a;
            if ((_a = this._parent) === null || _a === void 0 ? void 0 : _a.classList.contains(CLASS_NAME_MAXIMIZED)) {
                this.minimize();
                return;
            }
            this.maximize();
        }
    }
    /**
     *
     * Data Api implementation
     * ====================================================
     */
    domReady(() => {
        const collapseBtn = document.querySelectorAll(SELECTOR_DATA_COLLAPSE);
        for (const btn of collapseBtn) {
            btn.addEventListener('click', event => {
                event.preventDefault();
                const target = event.target;
                const data = new CardWidget(target, Default);
                data.toggle();
            });
        }
        const removeBtn = document.querySelectorAll(SELECTOR_DATA_REMOVE);
        for (const btn of removeBtn) {
            btn.addEventListener('click', event => {
                event.preventDefault();
                const target = event.target;
                const data = new CardWidget(target, Default);
                data.remove();
            });
        }
        const maxBtn = document.querySelectorAll(SELECTOR_DATA_MAXIMIZE);
        for (const btn of maxBtn) {
            btn.addEventListener('click', event => {
                event.preventDefault();
                const target = event.target;
                const data = new CardWidget(target, Default);
                data.toggleMaximize();
            });
        }
    });

    exports.CardWidget = CardWidget;
    exports.DirectChat = DirectChat;
    exports.Layout = Layout;
    exports.PushMenu = PushMenu;
    exports.Treeview = Treeview;

}));
//# sourceMappingURL=adminlte.js.map
