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
    private MapType currentMapTypeId = MapType.MAIN_SCREEN_MAIN_MAP;

    private MapTypeManager() {
    }

    private static final class Holder {
        static final MapTypeManager instance = new MapTypeManager();
    }

    public static MapTypeManager getInstance() {
        return Holder.instance;
    }

    public MapType getCurrentMapTypeId() {
        Logger.d(TAG, "getCurrentMapTypeId:" + currentMapTypeId.name());
        return currentMapTypeId;
    }

    public void setCurrentMapTypeId(MapType currentMapTypeId) {
        this.currentMapTypeId = currentMapTypeId;
        Logger.d(TAG, "setCurrentMapTypeId:" + currentMapTypeId.name());
    }

    public MapType getMapTypeIdByName(@Nullable String name) {
        if (TextUtils.equals(name, MapType.LAUNCHER_DESK_MAP.name())) {
            return MapType.LAUNCHER_DESK_MAP;
        } else if (TextUtils.equals(name, MapType.LAUNCHER_WIDGET_MAP.name())) {
            return MapType.LAUNCHER_WIDGET_MAP;
        }if (TextUtils.equals(name, MapType.CLUSTER_MAP.name())) {
            return MapType.CLUSTER_MAP;
        }  else {
            return MapType.MAIN_SCREEN_MAIN_MAP;
        }
    }
}
