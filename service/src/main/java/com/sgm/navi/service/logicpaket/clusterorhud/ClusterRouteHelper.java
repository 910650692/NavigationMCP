package com.sgm.navi.service.logicpaket.clusterorhud;

import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.sgm.navi.service.adapter.navi.NaviAdapter;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;

import java.util.ArrayList;

public class ClusterRouteHelper {
    public static final String TAG = "ClusterRouteHelper";
    private static final MapType MAIN_MAP_TYPE = MapType.MAIN_SCREEN_MAIN_MAP;

    public static void refreshPathList() {
        Logger.i(TAG, "Refreshing path list...");
        long currentPathID = OpenApiHelper.getCurrentPathId(MapType.MAIN_SCREEN_MAIN_MAP);
        ArrayList<PathInfo> pathInfos = new ArrayList<>(OpenApiHelper.getCurrentPathInfos());
        int mainPathIndex = NumberUtils.NUM_ERROR;

        if (pathInfos.isEmpty()) {
            Logger.i(TAG, "refreshPathList: pathInfos is empty");
            return;
        }

        for (int i = 0; i < pathInfos.size(); i++) {
            if (pathInfos.get(i).getPathID() == currentPathID) {
                mainPathIndex = i;
                Logger.i(TAG, "Main path found at index: " + mainPathIndex);
                break;
            }
        }

        if (mainPathIndex != NumberUtils.NUM_ERROR) {
            Logger.i(TAG, "Updating path info with main path index: " + mainPathIndex);
            NaviAdapter.getInstance().updatePathInfo(MapType.CLUSTER_MAP, pathInfos, mainPathIndex);
        } else {
            Logger.w(TAG, "Main path not found in the list. Path ID: " + currentPathID);
        }
    }

    /**
     * 显示当前主路线的路径
     */
    public static void onlyShowCurrentPath(MapType mapType) {
        Logger.i(TAG, "cluster onlyShowCurrentPath");
        PathInfo pathInfo = OpenApiHelper.getCurrentPathInfo(MAIN_MAP_TYPE);
        if (null == pathInfo) {
            Logger.i(TAG, "main onlyShowCurrentPath pathInfo is null");
            return;
        }
        ArrayList<PathInfo> pathInfoList = new ArrayList<>();
        pathInfoList.add(pathInfo);
        NaviAdapter.getInstance().updatePathInfo(mapType, pathInfoList, 0);
    }
}
