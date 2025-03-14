package com.fy.navi.service.define.map;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;

/**
 * Author: QiuYaWei
 * Date: 2025/2/19
 * Description: [在这里描述文件功能]
 */
public class MapTypeManager {
    private static final String TAG = "MapTypeManager";
    private MapTypeId currentMapTypeId = MapTypeId.MAIN_SCREEN_MAIN_MAP;

    private MapTypeManager() {
    }

    private static final class Holder {
        static final MapTypeManager instance = new MapTypeManager();
    }

    public static MapTypeManager getInstance() {
        return Holder.instance;
    }

    public MapTypeId getCurrentMapTypeId() {
        Logger.d(TAG, "getCurrentMapTypeId:" + currentMapTypeId.name());
        return currentMapTypeId;
    }

    public void setCurrentMapTypeId(MapTypeId currentMapTypeId) {
        this.currentMapTypeId = currentMapTypeId;
        Logger.d(TAG, "setCurrentMapTypeId:" + currentMapTypeId.name());
    }

    public MapTypeId getMapTypeIdByName(@Nullable String name) {
        if (TextUtils.equals(name, MapTypeId.LAUNCHER_DESK_MAP.name())) {
            return MapTypeId.LAUNCHER_DESK_MAP;
        } else if (TextUtils.equals(name, MapTypeId.LAUNCHER_WIDGET_MAP.name())) {
            return MapTypeId.LAUNCHER_WIDGET_MAP;
        } else {
            return MapTypeId.MAIN_SCREEN_MAIN_MAP;
        }
    }
}
