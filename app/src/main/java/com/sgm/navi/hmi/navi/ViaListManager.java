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
            List<String> mViaIdList = new ArrayList<>();
            final List<NaviViaEntity> viaList = mGuidanceModel.getViaList();
            if (!ConvertUtils.isEmpty(viaList)) {
                for (int i = 0; i < viaList.size(); i++) {
                    NaviViaEntity naviViaEntity = viaList.get(i);
                    if (naviViaEntity != null && naviViaEntity.isUserAdd()) {
                        Logger.d(TAG, "id:", naviViaEntity.getPid(), " name:", naviViaEntity.getName(), " charge:", naviViaEntity.getChargeInfo());
                        mViaIdList.add(naviViaEntity.getPid());
                    }
                }
            }
            if (!ConvertUtils.isEmpty(mViaIdList)) {
                mChargeStationTaskId = mSearchPackage.poiListSearch(mViaIdList, 4, true);
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
}
