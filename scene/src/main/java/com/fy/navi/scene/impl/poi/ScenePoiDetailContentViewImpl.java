package com.fy.navi.scene.impl.poi;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.text.TextUtils;
import android.util.Pair;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.poi.IScenePoiDetailContentView;
import com.fy.navi.scene.ui.poi.ScenePoiDetailContentView;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.concurrent.CompletableFuture;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class ScenePoiDetailContentViewImpl extends BaseSceneModel<ScenePoiDetailContentView> implements IScenePoiDetailContentView {
    private final SearchPackage mSearchPackage;
    private final BehaviorPackage behaviorPackage;
    private final MapDataPackage mapDataPackage;


    public ScenePoiDetailContentViewImpl(ScenePoiDetailContentView mScreenView) {
        super(mScreenView);
        mSearchPackage = SearchPackage.getInstance();
        behaviorPackage = BehaviorPackage.getInstance();
        this.mapDataPackage = MapDataPackage.getInstance();
    }

    @Override
    public void closeFragment() {
        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
        // 清除扎标的点
        mSearchPackage.clearLabelMark();
    }

    public CityDataInfo getCityInfo(int acCode) {
        return mapDataPackage.getCityInfo(acCode);
    }

    public int getAcCode() {
        return mSearchPackage.getAcCode();
    }


    @Override
    public void doSearch(PoiInfoEntity poiInfoEntity) {
        if (null == poiInfoEntity) {
            Logger.d(SEARCH_HMI_TAG, "doSearch: poiInfoEntity is null");
            return;
        }
        // 这里只有两种搜索类型：POI搜索和Geo搜索
        if (!TextUtils.isEmpty(poiInfoEntity.getPid())) {
            mSearchPackage.poiIdSearch(poiInfoEntity.getPid());
            //POI详情搜索测试代码，正式版本sdk时放开
//            mSearchPackage.poiDetailSearch(poiInfoEntity, poiInfoEntity.getRetainParam());
        } else {
            mSearchPackage.geoSearch(poiInfoEntity.getPoint());
        }
    }

    public GeoPoint getCurrentLocation() {
        return mSearchPackage.getCurrentLocation();
    }

    /**
     * 是否收藏
     *
     * @param poiInfo PoiInfoEntity
     * @return true：已收藏，false：未收藏
     */
    public String isFavorite(PoiInfoEntity poiInfo) {
        return behaviorPackage.isFavorite(poiInfo);
    }

    /**
     * 添加POI收藏点
     *
     * @param poiInfo PoiInfoEntity
     * @return id：成功，null：失败
     */
    public String addFavorite(PoiInfoEntity poiInfo) {
        return behaviorPackage.addFavorite(poiInfo);
    }

    /**
     * 删除POI收藏点
     *
     * @param poiInfo PoiInfoEntity
     * @return id：成功，null：失败
     */
    public String removeFavorite(PoiInfoEntity poiInfo) {
        return behaviorPackage.removeFavorite(poiInfo);
    }

    /**
     * 添加收藏夹信息到本地数据库
     *
     * @param entity
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void addFavoriteData(PoiInfoEntity entity, int favoriteType) {
        behaviorPackage.addFavoriteData(entity, favoriteType);
    }

    /**
     * 删除 itemId 对应的本地数据
     *
     * @param itemId 收藏点唯一码
     */
    public void deleteFavoriteData(String itemId) {
        behaviorPackage.deleteFavoriteData(itemId);
    }

    /**
     * 查找 itemId 对应的本地数据是否为收藏点
     *
     * @param itemId 收藏点唯一码
     * @return true 已收藏，false 未收藏
     */
    public boolean isFavoriteData(String itemId) {
        return behaviorPackage.isFavorite(itemId);
    }

    public boolean isAlongWaySearch() {
        return mSearchPackage.isAlongWaySearch();
    }

    public int getPointTypeCode(String typeCode) {
        return mSearchPackage.getPointTypeCode(typeCode);
    }

    /**
     * 获取预计到达时间
     *
     * @param geoPoint 目标点经纬度
     * @return distance ，travelTime
     */
    public CompletableFuture<ETAInfo> getTravelTimeFuture(GeoPoint geoPoint) {
        return mSearchPackage.getTravelTimeFutureIncludeChargeLeft(geoPoint);
    }
}
