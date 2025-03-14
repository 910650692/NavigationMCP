package com.fy.navi.service.adapter.user.behavior;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.Objects;

/**
 * @Description
 * @Author fh
 * @date 2024/12/26
 */
public class BehaviorAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(BehaviorAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "BehaviorAdapterImpl";
    private IBehaviorApi mBehaviorApi;

    private BehaviorAdapter() {
        mBehaviorApi = (IBehaviorApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initBehaviorService() {
        mBehaviorApi.initBehaviorService();
    }

    public void registerCallBack(String key, BehaviorAdapterCallBack callBack) {
        mBehaviorApi.registerCallBack(key, callBack);
    }

    public void removeCallBack(String key) {
        mBehaviorApi.unRegisterCallback(key);
    }

    public void unInitBehaviorService() {
        mBehaviorApi.unInitBehaviorService();
    }

    public int[] getSimpleFavoriteIds() {
        return mBehaviorApi.getSimpleFavoriteIds();
    }

    public PoiInfoEntity getHomeFavoriteInfo() {
        return mBehaviorApi.getHomeFavoriteInfo();
    }

    public PoiInfoEntity getCompanyFavoriteInfo() {
        return mBehaviorApi.getCompanyFavoriteInfo();
    }

    public  ArrayList<PoiInfoEntity> getSimpleFavoriteList() {
        return mBehaviorApi.getSimpleFavoriteList();
    }

    public PoiInfoEntity getFavorite(PoiInfoEntity baseInfo) {
       return mBehaviorApi.getFavorite(baseInfo);
    }

    public String addFavorite(PoiInfoEntity poiInfo) {
        return mBehaviorApi.addFavorite(poiInfo);
    }

    public int getFavoriteListAsync(int type, boolean sorted) {
       return mBehaviorApi.getFavoriteListAsync(type, sorted);
    }

    public String removeFavorite(PoiInfoEntity poiInfo) {
       return mBehaviorApi.removeFavorite(poiInfo);
    }

    public String isFavorite(PoiInfoEntity poiInfo) {
       return mBehaviorApi.isFavorite(poiInfo);
    }

    public String topFavorite(PoiInfoEntity baseInfo, boolean bSetTop) {
        return mBehaviorApi.topFavorite(baseInfo, bSetTop);
    }

    public String modifyFavorite(PoiInfoEntity detailInfo, String customName) {
       return mBehaviorApi.modifyFavorite(detailInfo, customName);
    }

    public void startSync() {
        mBehaviorApi.startSync();
    }

    public static BehaviorAdapter getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final BehaviorAdapter ra = new BehaviorAdapter();
    }
}
