package com.sgm.navi.hmi.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;

import java.util.ArrayList;
import java.util.List;

public class ViaListManager {
    private static final String TAG = "ViaListManager";
    private final NaviGuidanceModel mGuidanceModel;
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    //途经点充电站信息请求控制
    private int mViaUpdateCount;
    private int mChargeStationTaskId;

    public ViaListManager(final NaviGuidanceModel naviGuidanceModel) {
        mGuidanceModel = naviGuidanceModel;
        mSearchPackage = SearchPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
    }

    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        if (mViaUpdateCount == 0) {
            List<String> viaIdList = getViaIdList(mGuidanceModel.getViaList());
            if (!ConvertUtils.isEmpty(viaIdList)) {
                mChargeStationTaskId = mSearchPackage.poiListSearch(viaIdList, 4, true);
            }
        }
        mViaUpdateCount++;
        if (mViaUpdateCount > 3 * 60) {
            mViaUpdateCount = 0;
        }
    }

    /**
     * 详情搜索结果
     *
     * @param taskId
     * @param errorCode
     * @param message
     * @param searchResultEntity
     */
    public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (mChargeStationTaskId != taskId) {
            return;
        }
        if (ConvertUtils.isEmpty(searchResultEntity)) {
            Logger.i(TAG, "errorCode:", errorCode, "message:", message, " 搜索结果:isEmpty(searchResultEntity)");
            return;
        }
        List<PoiInfoEntity> poiInfos = searchResultEntity.getPoiList();
        if (ConvertUtils.isEmpty(poiInfos)) {
            Logger.i(TAG, "errorCode:", errorCode, "message:", message, " 搜索结果:isEmpty(poiInfos)");
            return;
        }
        mRoutePackage.updateViaPointList(MapType.MAIN_SCREEN_MAIN_MAP, poiInfos);
    }

    /**
     * 途经点信息更新
     *
     * @param viaList
     */
    public void updateViaList(final List<NaviViaEntity> viaList) {
        mViaUpdateCount = 1; // 重置更新计数
        if (ConvertUtils.isEmpty(viaList)) {
            return;
        }
        List<String> viaIdList = getViaIdList(viaList);
        if (!ConvertUtils.isEmpty(viaIdList)) {
            mChargeStationTaskId = mSearchPackage.poiListSearch(viaIdList, 4, true);
        }
    }

    /**
     * 获取用户添加的途经点ID列表
     * @param viaList
     * @return
     */
    private List<String> getViaIdList(List<NaviViaEntity> viaList) {
        if (ConvertUtils.isEmpty(viaList)) {
            return null;
        }
        List<String> viaIdList = new ArrayList<>();
        for (int i = 0; i < viaList.size(); i++) {
            NaviViaEntity naviViaEntity = viaList.get(i);
            if (naviViaEntity != null && naviViaEntity.isUserAdd()) {
                viaIdList.add(naviViaEntity.getPid());
            }
        }
        if (Logger.openLog) {
            Logger.d(TAG, viaIdList);
        }
        return viaIdList;
    }


}
