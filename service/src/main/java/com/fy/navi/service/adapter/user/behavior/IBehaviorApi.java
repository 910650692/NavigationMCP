package com.fy.navi.service.adapter.user.behavior;

import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

public interface IBehaviorApi {

    /**
     * initBehaviorService
     */
    void initBehaviorService();

    /**
     * 设置账号信息
     */
    void setLoginInfo();

    /**
     * registerCallBack
     * @param key
     * @param callBack
     */
    void registerCallBack(String key, BehaviorAdapterCallBack callBack);

    /**
     * unRegisterCallback
     * @param key
     */
    void unRegisterCallback(String key);

    /**
     * unInitBehaviorService
     */
    void unInitBehaviorService();

    /**
     * getSimpleFavoriteIds
     * @return int[]
     */
    int[] getSimpleFavoriteIds();

    /**
     * getHomeFavoriteInfo
     * @return entity
     */
    PoiInfoEntity getHomeFavoriteInfo();

    /**
     * getCompanyFavoriteInfo
     * @return entity
     */
    PoiInfoEntity getCompanyFavoriteInfo();

    /**
     * getSimpleFavoriteList
     * @return entity
     */
    ArrayList<PoiInfoEntity> getSimpleFavoriteList();

    /**
     * getFavorite
     * @param item
     * @return entity
     */
    PoiInfoEntity getFavorite(PoiInfoEntity item);

    /**
     * addFavorite
     * @param poiInfo
     * @return string
     */
    String addFavorite(PoiInfoEntity poiInfo);

    /**
     * getFavoriteListAsync
     * @param type
     * @param sorted
     * @return int
     */
    int getFavoriteListAsync(int type, boolean sorted);

    /**
     * removeFavorite
     * @param poiInfo
     * @return string
     */
    String removeFavorite(PoiInfoEntity poiInfo);

    /**
     * isFavorite
     * @param poiInfo
     * @return string
     */
    String isFavorite(PoiInfoEntity poiInfo);

    /**
     * topFavorite
     * @param baseInfo
     * @param isSetTop
     * @return string
     */
    String topFavorite(PoiInfoEntity baseInfo, boolean isSetTop);

    /**
     * modifyFavorite
     * @param baseInfo
     * @param customName
     * @return string
     */
    String modifyFavorite(PoiInfoEntity baseInfo, String customName);

    /**
     * addFrequentAddress
     * @param poiInfo
     * @return string
     */
    String addFrequentAddress(PoiInfoEntity poiInfo);

    /**
     * startSync
     */
    void startSync();

}
