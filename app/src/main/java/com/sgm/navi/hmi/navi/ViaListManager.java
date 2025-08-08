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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ViaListManager {
    private static final String TAG = "ViaListManager";
    private final NaviGuidanceModel mGuidanceModel;
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    //途经点充电站信息请求控制
    private int mViaUpdateCount;
    private int mChargeStationTaskId;
    private List<String> mViaIdList;
    private List<NaviViaEntity> mNaviViaList;
    private List<PoiInfoEntity> mGuideRouteViaList;

    public ViaListManager(final NaviGuidanceModel naviGuidanceModel) {
        mGuidanceModel = naviGuidanceModel;
        mSearchPackage = SearchPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
    }

    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        if (mViaUpdateCount == 0) {
            mNaviViaList = mGuidanceModel.getViaList();
            mViaIdList = getViaIdList(mNaviViaList);
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
        mNaviViaList = mGuidanceModel.getViaList();
        mViaIdList = getViaIdList(mNaviViaList);
        List<PoiInfoEntity> tempPoiInfos = sortPoiInfosByIds(poiInfos, mViaIdList, mNaviViaList);
        mGuideRouteViaList = tempPoiInfos;
        if (Logger.openLog) {
            Logger.d(TAG, " poiInfos:", tempPoiInfos.size(), " viaIdList:", mViaIdList == null ? "null" : mViaIdList.size(),
                    " naviViaList:", mNaviViaList == null ? "null" : mNaviViaList.size());
        }
        if (!ConvertUtils.isEmpty(tempPoiInfos)) {
            mRoutePackage.updateViaPointList(MapType.MAIN_SCREEN_MAIN_MAP, tempPoiInfos);
        }
    }

    public void updateViaPointList() {
        if (!ConvertUtils.isEmpty(mGuideRouteViaList)) {
            mNaviViaList = mGuidanceModel.getViaList();
            mViaIdList = getViaIdList(mNaviViaList);
            List<PoiInfoEntity> tempPoiInfos = sortPoiInfosByIds(mGuideRouteViaList, mViaIdList, mNaviViaList);
            mGuideRouteViaList = tempPoiInfos;
            if (!ConvertUtils.isEmpty(tempPoiInfos)) {
                mRoutePackage.updateViaPointList(MapType.MAIN_SCREEN_MAIN_MAP, tempPoiInfos);
            }
        }
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
        mNaviViaList = viaList;
        mViaIdList = getViaIdList(viaList);
        if (!ConvertUtils.isEmpty(mViaIdList)) {
            mChargeStationTaskId = mSearchPackage.poiListSearch(mViaIdList, 4, true);
        }
    }

    /**
     * 获取用户添加的途经点ID列表
     * @param viaList
     * @return
     */
    private List<String> getViaIdList(List<NaviViaEntity> viaList) {
        List<String> viaIdList = new ArrayList<>();
        if (ConvertUtils.isEmpty(viaList)) {
            return viaIdList;
        }
        for (int i = 0; i < viaList.size(); i++) {
            NaviViaEntity naviViaEntity = viaList.get(i);
            if (naviViaEntity != null && naviViaEntity.isUserAdd() && !naviViaEntity.isEndPoi()) {
                viaIdList.add(naviViaEntity.getPid());
            }
        }
        if (Logger.openLog) {
            Logger.d(TAG, viaIdList);
        }
        return viaIdList;
    }

    /**
     * 根据poi id数组顺序对PoiInfoEntityList和naviViaList进行排序整理
     * @param poiInfoEntityList poiInfoEntityList
     * @param poiIdList poi id列表
     * @param naviViaList 导航途径点列表
     * @return 排序后的PoiInfoEntity列表
     */
    private List<PoiInfoEntity> sortPoiInfosByIds(List<PoiInfoEntity> poiInfoEntityList, List<String> poiIdList,
                                                  List<NaviViaEntity> naviViaList) {
        if (ConvertUtils.isEmpty(poiInfoEntityList) || ConvertUtils.isEmpty(poiIdList)
                || ConvertUtils.isEmpty(naviViaList)) {
            Logger.d(TAG, "poiInfoEntityList poiIdList naviViaList长度都不能为空");
            return new ArrayList<>(poiInfoEntityList);
        }
        Map<String, PoiInfoEntity> idToObject = new HashMap<>();
        for(PoiInfoEntity poiInfoEntity: poiInfoEntityList) {
            idToObject.put(poiInfoEntity.getPid(), poiInfoEntity);
        }
        Map<String, NaviViaEntity> idToNaviViaEntity= new HashMap<>();
        for(NaviViaEntity naviViaEntity: naviViaList) {
            idToNaviViaEntity.put(naviViaEntity.getPid(), naviViaEntity);
        }
        List<PoiInfoEntity> resultList = new ArrayList<>();
        for(int i = 0; i < poiIdList.size(); i++) {
            if(idToObject.containsKey(poiIdList.get(i))) {
                resultList.add(idToObject.get(poiIdList.get(i)));
            } else {// 解决poi id不正确的途径点
                NaviViaEntity naviViaEntity = idToNaviViaEntity.get(poiIdList.get(i));
                PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                if (!ConvertUtils.isEmpty(naviViaEntity)) {
                    poiInfoEntity.setPid(naviViaEntity.getPid());
                    poiInfoEntity.setMName(naviViaEntity.getName());
                    poiInfoEntity.setAddress(naviViaEntity.getAddress());
                    poiInfoEntity.setMPoint(naviViaEntity.getRealPos());
                }
                resultList.add(poiInfoEntity);
            }
        }
        return resultList;
    }

}
