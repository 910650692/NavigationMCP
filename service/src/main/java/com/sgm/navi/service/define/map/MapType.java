package com.sgm.navi.service.define.map;

import lombok.Getter;

public enum MapType {
    MAIN_SCREEN_MAIN_MAP(1),
    LAUNCHER_DESK_MAP(3),
    WIDGET_MAP(5),  //预留的纯路口大图
    HUD_MAP(7),
    CLUSTER_MAP(9),
    LAUNCHER_WIDGET_MAP(11),
    REAR_SCREEN_MAP(3);//后排吸顶屏,暂时复用3

    @Getter
    private int mapType;

    MapType(int mapType) {
        this.mapType = mapType;
    }
}
