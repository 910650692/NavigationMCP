package com.fy.navi.scene.impl.poi;


import android.app.Activity;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.lifecycle.MutableLiveData;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.poi.IScenePoiDetailContentView;
import com.fy.navi.scene.ui.poi.ScenePoiDetailContentView;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.user.account.AccessTokenParam;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class ScenePoiDetailContentViewImpl extends BaseSceneModel<ScenePoiDetailContentView> implements IScenePoiDetailContentView {
    private final SearchPackage mSearchPackage;
    private final BehaviorPackage mBehaviorPackage;
    private final MapDataPackage mapDataPackage;
    private final RoutePackage mRoutePackage;
    private int mTaskId;
    // 动力类型标定
    public MutableLiveData<Integer> mPowerType = new MutableLiveData<>();

    /**
     * 获取高德SDK请求任务Id
     * @return 请求任务Id
     */
    public int getMTaskId() {
        return mTaskId;
    }

    public ScenePoiDetailContentViewImpl(final ScenePoiDetailContentView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
        mBehaviorPackage = BehaviorPackage.getInstance();
        mapDataPackage = MapDataPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mPowerType.setValue(-1);
    }

    @Override
    public void closeFragment() {
        boolean isOpenFromNavi = mScreenView != null && mScreenView.getIsOpenFromNavi();
        if (!isOpenFromNavi) {
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        } else {
            Bundle bundle = new Bundle();
            bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
            StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(bundle);
        }
        // 清除扎标的点
        mSearchPackage.clearPoiLabelMark();
        mSearchPackage.clearTypeMark(LayerPointItemType.SEARCH_CHILD_POINT);
        mSearchPackage.clearTypeMark(LayerPointItemType.SEARCH_PARENT_Line_Road);
        mSearchPackage.clearTypeMark(LayerPointItemType.SEARCH_PARENT_AREA);
        mSearchPackage.clearFocus();

    }

    /**
     * 清除指定类型的扎标
     * @param type 扎标类型
     */
    public void clearTypeMark(final LayerPointItemType type) {
        mSearchPackage.clearTypeMark(type);
    }

    /**
     * 清除搜索图层所有扎标
     */
    public void clearLabelMarker() {
        mSearchPackage.clearLabelMark();
    }

    /**
     * 获取城市信息
     *
     * @param acCode 城市编码
     * @return CityDataInfo
     */
    public CityDataInfo getCityInfo(final int acCode) {
        return mapDataPackage.getCityInfo(acCode);
    }

    public int getAcCode() {
        return mSearchPackage.getAcCode();
    }


    @Override
    public void doSearch(final PoiInfoEntity poiInfoEntity) {
        if (null == poiInfoEntity) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "doSearch: poiInfoEntity is null");
            return;
        }
        // 这里只有两种搜索类型：POI搜索和Geo搜索,带”."的是逆地理搜索自行拼接的pid，不可用于逆地理搜索
        if (!TextUtils.isEmpty(poiInfoEntity.getPid()) && !poiInfoEntity.getPid().contains(".")
                && poiInfoEntity.getPid().startsWith("B")) {
            if (!ConvertUtils.isEmpty(poiInfoEntity.getPoint())) {
                mTaskId = mSearchPackage.poiIdSearch(poiInfoEntity.getPid(), poiInfoEntity.getPoint());
            } else {
                mTaskId = mSearchPackage.poiIdSearch(poiInfoEntity.getPid());
            }
            //POI详情搜索测试代码，正式版本sdk时放开
//            mSearchPackage.poiDetailSearch(poiInfoEntity, poiInfoEntity.getRetainParam());
        } else {
            mTaskId = mSearchPackage.geoSearch(poiInfoEntity.getPoint());
        }
    }

    public void keywordSearch(PoiInfoEntity poiInfo){
        mTaskId = mSearchPackage.keywordSearch(1,poiInfo.getName(),true);
    }

    // 云端自营站查询详情
    public void doSearchByNet(final PoiInfoEntity poiInfoEntity){
        mTaskId = mSearchPackage.queryStationInfo(poiInfoEntity);
    }

    public GeoPoint getCurrentLocation() {
        return mSearchPackage.getCurrentLocation();
    }

    /**
     * 是否收藏
     *
     * @param poiInfo PoiInfoEntity
     * @return itemId 已经收藏 null 未收藏
     */
    public String isFavorite(final PoiInfoEntity poiInfo) {
        return mBehaviorPackage.isFavorite(poiInfo);
    }

    /**
     * 是否常去地址
     *
     * @param poiInfo PoiInfoEntity
     * @return 是否已经收藏
     */
    public boolean isFrequentAddress(final PoiInfoEntity poiInfo) {
        return mBehaviorPackage.isFrequentAddress(poiInfo);
    }

    /**
     * 添加POI收藏点
     *
     * @param poiInfo PoiInfoEntity
     * @return id：成功，null：失败
     */
    public String addFavorite(final PoiInfoEntity poiInfo, final int type) {
        return mBehaviorPackage.addFavorite(poiInfo, type);
    }

    /**
     * 删除POI收藏点
     *
     * @param poiInfo PoiInfoEntity
     * @return id：成功，null：失败
     */
    public String removeFavorite(final PoiInfoEntity poiInfo) {
        return mBehaviorPackage.removeFavorite(poiInfo);
    }

    /**
     * 添加poi标记
     * @param poiInfoEntities 搜索结果列表
     * @param index 选中下标
     */
    public void addPoiMarker(final List<PoiInfoEntity> poiInfoEntities, final int index) {
        mSearchPackage.createPoiMarker(poiInfoEntities, index);
    }

    /**
     * 添加poi标记
     * @param searchResultEntity poi扎标对象
     */
    public void createLabelMarker(final SearchResultEntity searchResultEntity) {
        if (searchResultEntity != null) {
            mSearchPackage.createLabelMarker(searchResultEntity);
        }
    }

    /**
     * 清除POI点所有相关扎标
     */
    public void clearAllPoiMarker() {
        if (mSearchPackage != null) {
            mSearchPackage.clearPoiLabelMark();
            mSearchPackage.clearTypeMark(LayerPointItemType.SEARCH_CHILD_POINT);
            mSearchPackage.clearTypeMark(LayerPointItemType.SEARCH_PARENT_Line_Road);
            mSearchPackage.clearTypeMark(LayerPointItemType.SEARCH_PARENT_AREA);
        }
    }

    /**
     * 设置子节点高亮下标
     * @param index 高亮下标
     */
    public void setChildIndex(final int index) {
        mSearchPackage.setChildIndex(index);
    }

    /**
     * 添加收藏夹信息到本地数据库
     *
     * @param entity       收藏点信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void addFavoriteData(final PoiInfoEntity entity, final int favoriteType) {
        mBehaviorPackage.addFavoriteData(entity, favoriteType);
    }

    /**
     * 删除 itemId 对应的本地数据
     *
     * @param itemId 收藏点唯一码
     */
    public void deleteFavoriteData(final String itemId) {
//        mBehaviorPackage.deleteFavoriteData(itemId);
    }

    /**
     * 查找 itemId 对应的本地数据是否为收藏点
     *
     * @param itemId 收藏点唯一码
     * @return true 已收藏，false 未收藏
     */
    public boolean isFavoriteData(final String itemId) {
        return mBehaviorPackage.isFavorite(itemId);
    }

    public boolean isAlongWaySearch() {
        return mSearchPackage.isAlongWaySearch();
    }

    /**
     * 获取POI点类型编码
     *
     * @param typeCode POI的typeCode
     * @return 对应类型
     */
    public int getPointTypeCode(final String typeCode) {
        return mSearchPackage.getPointTypeCode(typeCode);
    }

    /**
     * 获取预计到达时间
     *
     * @param geoPoint 目标点经纬度
     * @return distance ，travelTime
     * @return distance ，travelTime
     */
    public CompletableFuture<ETAInfo> getTravelTimeFuture(final GeoPoint geoPoint) {
        return mSearchPackage.getTravelTimeFutureIncludeChargeLeft(geoPoint);
    }

    /**
     * 更新子点孙节点列表数据
     * @param childInfo 当前子点
     * @return 携带孙节点数据的子点
     */
    public CompletableFuture<ChildInfo> setGrandChildInfoList(final ChildInfo childInfo) {
        return mSearchPackage.setGrandChildInfoList(childInfo);
    }

    /**
     * 获取当前已添加的途径点数量
     *
     * @return 途径点数量
     */
    public int getViaCount() {
        return mRoutePackage.getViaPointsCount(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    public void updateCollectStatus(Activity activity,PoiInfoEntity poiInfo) {
        AccessTokenParam param = new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                activity,
                null,
                null,
                null,
                null);

        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String vehicleBrand = "BUICK";
            mSearchPackage.updateCollectStatus(idpUserId,accessToken,vehicleBrand,poiInfo);
        });
    }

    // 判断SGM是否已登陆
    public boolean isSGMLogin(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"isSGMLogin: "+ AccountPackage.getInstance().isSGMLogin());
        return AccountPackage.getInstance().isSGMLogin();
    }

    public void startSGMLogin(){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"startSGMLogin");
        AccountPackage.getInstance().sendSGMLoginRequest(mScreenView.getContext());
    }
}
