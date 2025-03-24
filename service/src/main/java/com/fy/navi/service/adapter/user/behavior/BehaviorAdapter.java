package com.fy.navi.service.adapter.user.behavior;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.Objects;

final public class BehaviorAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(BehaviorAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "BehaviorAdapterImpl";
    private IBehaviorApi mBehaviorApi;

    private BehaviorAdapter() {
        mBehaviorApi = (IBehaviorApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    /**
     * initBehaviorService
     */
    public void initBehaviorService() {
        mBehaviorApi.initBehaviorService();
    }

    /**
     * registerCallBack
     * @param key
     * @param callBack
     */
    public void registerCallBack(final String key, final BehaviorAdapterCallBack callBack) {
        mBehaviorApi.registerCallBack(key, callBack);
    }

    /**
     * removeCallBack
     * @param key
     */
    public void removeCallBack(final String key) {
        mBehaviorApi.unRegisterCallback(key);
    }

    /**
     * unInitBehaviorService
     */
    public void unInitBehaviorService() {
        mBehaviorApi.unInitBehaviorService();
    }

    /**
     * getSimpleFavoriteIds
     * @return int[]
     */
    public int[] getSimpleFavoriteIds() {
        return mBehaviorApi.getSimpleFavoriteIds();
    }

    /**
     * getHomeFavoriteInfo
     * @return entity
     */
    public PoiInfoEntity getHomeFavoriteInfo() {
        return mBehaviorApi.getHomeFavoriteInfo();
    }

    /**
     * getCompanyFavoriteInfo
     * @return entity
     */
    public PoiInfoEntity getCompanyFavoriteInfo() {
        return mBehaviorApi.getCompanyFavoriteInfo();
    }

    /**
     * getSimpleFavoriteList
     * @return list
     */
    public  ArrayList<PoiInfoEntity> getSimpleFavoriteList() {
        return mBehaviorApi.getSimpleFavoriteList();
    }

    /**
     * getFavorite
     * @param baseInfo
     * @return entity
     */
    public PoiInfoEntity getFavorite(final PoiInfoEntity baseInfo) {
       return mBehaviorApi.getFavorite(baseInfo);
    }

    /**
     * addFavorite
     * @param poiInfo
     * @return string
     */
    public String addFavorite(final PoiInfoEntity poiInfo) {
        return mBehaviorApi.addFavorite(poiInfo);
    }

    /**
     * getFavoriteListAsync
     * @param type
     * @param sorted
     * @return string
     */
    public int getFavoriteListAsync(final int type, final boolean sorted) {
       return mBehaviorApi.getFavoriteListAsync(type, sorted);
    }

    /**
     * removeFavorite
     * @param poiInfo
     * @return string
     */
    public String removeFavorite(final PoiInfoEntity poiInfo) {
       return mBehaviorApi.removeFavorite(poiInfo);
    }

    /**
     * isFavorite
     * @param poiInfo
     * @return string
     */
    public String isFavorite(final PoiInfoEntity poiInfo) {
       return mBehaviorApi.isFavorite(poiInfo);
    }

    /**
     * topFavorite
     * @param baseInfo
     * @param isSetTop
     * @return string
     */
    public String topFavorite(final PoiInfoEntity baseInfo, final boolean isSetTop) {
        return mBehaviorApi.topFavorite(baseInfo, isSetTop);
    }

    /**
     * modifyFavorite
     * @param detailInfo
     * @param customName
     * @return string
     */
    public String modifyFavorite(final PoiInfoEntity detailInfo, final String customName) {
       return mBehaviorApi.modifyFavorite(detailInfo, customName);
    }

    /**
     * startSync
     */
    public void startSync() {
        mBehaviorApi.startSync();
    }

    public static BehaviorAdapter getInstance() {
        return Helper.BA;
    }

    private static final class Helper {
        private static final BehaviorAdapter BA = new BehaviorAdapter();
    }
}
