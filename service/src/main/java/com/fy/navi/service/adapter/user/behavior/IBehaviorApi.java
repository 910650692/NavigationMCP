package com.fy.navi.service.adapter.user.behavior;

import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/26
 */
public interface IBehaviorApi {

    void initBehaviorService();

    void registerCallBack(String key, BehaviorAdapterCallBack callBack);

    void unRegisterCallback(String key);

    void unInitBehaviorService();

    int[] getSimpleFavoriteIds();

    PoiInfoEntity getHomeFavoriteInfo();

    PoiInfoEntity getCompanyFavoriteInfo();

    ArrayList<PoiInfoEntity> getSimpleFavoriteList();

    PoiInfoEntity getFavorite(PoiInfoEntity item);

    String addFavorite(PoiInfoEntity poiInfo);

    int getFavoriteListAsync(int type, boolean sorted);

    String removeFavorite(PoiInfoEntity poiInfo);

    String isFavorite(PoiInfoEntity poiInfo);

    String topFavorite(PoiInfoEntity baseInfo, boolean bSetTop);

    String modifyFavorite(PoiInfoEntity baseInfo, String customName);

    String addFrequentAddress(PoiInfoEntity poiInfo);

    void startSync();

}
