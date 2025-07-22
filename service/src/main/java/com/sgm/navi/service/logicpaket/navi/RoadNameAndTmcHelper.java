package com.sgm.navi.service.logicpaket.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.model.TrafficStatus;
import com.autonavi.gbl.common.path.option.LinkInfo;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.SegmentInfo;
import com.sgm.navi.service.adapter.navi.NaviAdapter;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.RoadName;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * 获取所有的道路名称和对应道路Tmc的辅助类
 */
public class RoadNameAndTmcHelper {

    public static final String TAG = "RoadNameAndTmcHelper";

    public RoadName getAllRoadName(MapType mapType, RoadName roadName, long pathId) {
        if (!ConvertUtils.isEmpty(roadName) && roadName.getPathId() == pathId) {
            return roadName;
        }
        return getAllRoadName(mapType, pathId);
    }

    private RoadName getAllRoadName(MapType mapType, long pathId) {
        PathInfo pathInfo = OpenApiHelper.getPathInfo(mapType, pathId);
        if (pathInfo != null) {
            RoadName roadName = new RoadName();
            HashMap<String, Integer> roadNameMap = new HashMap<>();
            long segmentCount = pathInfo.getSegmentCount();
            // 遍历所有的导航段
            for (long i = 0; i < segmentCount; i++) {
                SegmentInfo segmentInfo = pathInfo.getSegmentInfo(i);
                int segmentIndex = segmentInfo.getSegmentIndex();
                long linkCount = segmentInfo.getLinkCount();
                LinkInfo linkInfoLongest = segmentInfo.getLinkInfo(0);
                // 根据SDK的建议，取最长的link作为导航段的道路名称
                for (long j = 1; j < linkCount; j++) {
                    LinkInfo linkInfo = segmentInfo.getLinkInfo(j);
                    if (linkInfo.getLength() > linkInfoLongest.getLength()) {
                        linkInfoLongest = linkInfo;
                    }
                }
                Logger.i(TAG, "roadName:" + linkInfoLongest.getRoadName() +
                        " segmentIndex:" + segmentIndex);
                roadNameMap.put(linkInfoLongest.getRoadName(), segmentIndex);
            }
            roadName.setRoadNameMap(roadNameMap);
            roadName.setPathId(pathId);
            return roadName;
        }
        return null;
    }

    public int getTmcByRoadLinkIndex(NaviTmcInfo.NaviLightBarInfo currentLightBarInfo,
                                     int linkIndex) {
        final ArrayList<NaviTmcInfo.NaviLightBarItem> naviLightBarItems = currentLightBarInfo.
                getItemList();
        if (!ConvertUtils.isEmpty(naviLightBarItems)) {
            if (Logger.openLog) {
                Logger.i(TAG, "getTmcByRoadLinkIndex linkIndex:" + linkIndex +
                        " naviLightBarItems:" + naviLightBarItems);
            }
            int trafficStatus = TrafficStatus.TrafficStatusOpen;
            for (NaviTmcInfo.NaviLightBarItem naviLightBarItem : naviLightBarItems) {
                if (ConvertUtils.isNull(naviLightBarItem)) {
                    continue;
                }
                if (linkIndex >= naviLightBarItem.getStartSegmentIdx() &&
                        linkIndex <= naviLightBarItem.getEndSegmentIdx()) {
                   trafficStatus = NaviAdapter.getInstance().
                           getTrafficStatus(naviLightBarItem, trafficStatus);
                }
            }
            return trafficStatus;
        }
        return NumberUtils.NUM_ERROR;
    }

    /**
     * 精细化交通状态转换
     *
     * @param fineStatus 精细化交通状态
     * @return int交通状态
     */
    private int getFineStatus(final int fineStatus) {
        // 畅通
        if (fineStatus >= 100 && fineStatus < 200) {
            return TrafficStatus.TrafficStatusOpen;
            // 缓行
        } else if (fineStatus >= 200 && fineStatus < 300) {
            return TrafficStatus.TrafficStatusSlow;
            // 拥堵
        } else if (fineStatus >= 300 && fineStatus < 400) {
            return TrafficStatus.TrafficStatusJam;
        } else {
            return 0;
        }
    }
}
