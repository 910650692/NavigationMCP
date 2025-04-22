package com.fy.navi.service.define.map;

import lombok.Getter;

public enum MapType {
    MAIN_SCREEN_MAIN_MAP(1),
    LAUNCHER_DESK_MAP(3),
    LAUNCHER_WIDGET_MAP(5),
    HUD_MAP(7);
    @Getter
    private int mapType;

    MapType(int mapType) {
        this.mapType = mapType;
    }
}
