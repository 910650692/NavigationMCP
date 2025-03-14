package com.fy.navi.hmi.favorite;

import android.app.Application;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.hmi.route.RouteFragment;
import com.fy.navi.hmi.search.mainsearch.MainSearchFragment;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/23
 */
public class BaseFavoriteViewModel extends BaseViewModel<FavoriteFragment, FavoriteModel> {

    public MutableLiveData<Boolean> dataVisibility;
    public MutableLiveData<Boolean> tipVisibility;
    public MutableLiveData<Boolean> addVisibility;
    public MutableLiveData<Boolean> chargingVisibility;
    public MutableLiveData<Boolean> chargingNoDataVisibility;
    public MutableLiveData<Boolean> chargingRequestFailedVisibility;
    public MutableLiveData<Boolean> chargingOfflineVisibility;
    public MutableLiveData<Boolean> isHomeCompanyDisplayed;
    public MutableLiveData<Boolean> isEVCar;
    public PoiInfoEntity home;
    public PoiInfoEntity company;
    private boolean isHome;


    public BaseFavoriteViewModel(@NonNull Application application) {
        super(application);
        dataVisibility = new MutableLiveData<>(false);
        tipVisibility = new MutableLiveData<>(true);
        addVisibility = new MutableLiveData<>(false);
        chargingVisibility = new MutableLiveData<>(false);
        chargingNoDataVisibility = new MutableLiveData<>(false);
        chargingRequestFailedVisibility = new MutableLiveData<>(false);
        chargingOfflineVisibility = new MutableLiveData<>(false);
        isHomeCompanyDisplayed = new MutableLiveData<>(true);
        isEVCar = new MutableLiveData<>(false);
    }

    @Override
    protected FavoriteModel initModel() {
        return new FavoriteModel();
    }

    public void dualChoiceControl(String key, boolean isTrue) {
        switch (key) {
            case SettingController.KEY_SETTING_IS_EV_CAR:
                isEVCar.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED:
                isHomeCompanyDisplayed.setValue(isTrue);
                break;
        }
    }


    // 添加家
    public Action goSettingHome = () -> {
        Bundle bundle = new Bundle();
        if (home == null) {
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
            addFragment(new HomeCompanyFragment(), bundle);
        } else {
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, home);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            addFragment(new RouteFragment(), bundle);
        }
    };

    // 添加公司
    public Action goSettingCompany = () -> {
        Bundle bundle = new Bundle();
        if (company == null) {
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
            addFragment(new HomeCompanyFragment(), bundle);
        } else {
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, company);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            addFragment(new RouteFragment(), bundle);
        }
    };

    public Action showHomeCompanyDisplayed = () -> {
        boolean value = Boolean.FALSE.equals(isHomeCompanyDisplayed.getValue());
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, value);
        isHomeCompanyDisplayed.setValue(value);
        mModel.setHomeCompanyDisplay(value);
    };

    public boolean getIsHome() {
        return isHome;
    }

    public Action moreHome = () -> {
        isHome = true;
        mView.showPopupWindow(mView.getHomeOrCompanyEditView(true));
    };

    public Action moreCompany = () -> {
        isHome = false;
        mView.showPopupWindow(mView.getHomeOrCompanyEditView(false));
    };

    public Action addFavorite = () -> {
        addFragment(new MainSearchFragment(), null);
    };

    /**
     * 手动云服务同步
     */
    public Action favoriteSync = () -> {
        mModel.startSync();
    };

    public Action favoriteInfoClick = () -> {
        updateFavoriteView(mModel.getFavoritePoiData(0));
    };

    public Action chargingInfoClick = () -> {
        tipVisibility.setValue(false);
        dataVisibility.setValue(false);
        addVisibility.setValue(false);
        chargingVisibility.setValue(true);
    };


    /**
     * 获取收藏点列表
     */
    public void getSimpleFavoriteList() {
        mModel.getSimpleFavoriteList();
    }

    public void getFavoritePoiData() {
        mModel.getFavoritePoiData();
    }

    public void getHomeInfo() {
        mModel.getHomeInfo();
    }

    public void getCompanyInfo() {
        mModel.getCompanyInfo();
    }

    public void updateHomeView(PoiInfoEntity home) {
        this.home = home;
        mView.updateHomeView(home);
    }

    public void updateCompanyView(PoiInfoEntity company) {
        this.company = company;
        mView.updateCompanyView(company);
    }

    public void getSyncTime() {
        mModel.getSyncTime();
    }

    public void updateSyncTime(String syncTime) {
        mView.updateSyncTime(syncTime);
    }

    public void updateFavoritePoiData(ArrayList<PoiInfoEntity> list) {
        mView.updateFavoritePoiData(list);
    }

    public PoiInfoEntity getHomeCompanyInfo(boolean isHome) {
        if (isHome) {
            return home;
        } else {
            return company;
        }
    }

    public void initView() {
        mModel.initView();
    }

    public void setIsHomeCompanyDisplayed(boolean isHomeCompanyDisplayed) {
        this.isHomeCompanyDisplayed.setValue(isHomeCompanyDisplayed);
    }

    /**
     * 获取收藏点列表后更新UI
     *
     * @param list
     */

    public void updateFavoriteView(ArrayList<PoiInfoEntity> list) {

        chargingVisibility.setValue(false);
        chargingNoDataVisibility.setValue(false);
        chargingRequestFailedVisibility.setValue(false);
        chargingOfflineVisibility.setValue(false);

        if (list == null || list.isEmpty()) {
            tipVisibility.setValue(true);
            dataVisibility.setValue(false);
            addVisibility.setValue(false);
        } else {
            tipVisibility.setValue(false);
            dataVisibility.setValue(true);
            addVisibility.setValue(true);
            mView.updateFavoriteView(list);
        }
    }

    /**
     * 移除收藏点
     */
    public void removeFavorite(PoiInfoEntity poiInfo) {
        mModel.removeFavorite(poiInfo);
    }

    /**
     * 添加POI收藏点
     */
    public void addFavorite(String poiName, String poiId, int pointX, int pointY, int type) {
        PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setName(poiName);
        poiInfo.setPid(poiId);
        poiInfo.setPoint(new GeoPoint(pointX, pointY));
        FavoriteInfo favoriteInfo = new FavoriteInfo()
                .setCommonName(type);
        poiInfo.setFavoriteInfo(favoriteInfo); //  1，家  2，公司  0，普通收藏点;
        mModel.addFavorite(poiInfo);
    }

    /**
     * 收藏点重命名
     *
     * @param customName
     */
    public void modifyFavorite(String customName) {
        PoiInfoEntity favoriteInfo = new PoiInfoEntity();
        mModel.modifyFavorite(favoriteInfo, customName);
    }

    /**
     * 收藏点置顶/取消置顶
     *
     * @param poiInfoEntity
     * @param bSetTop       false 取消置顶
     */
    public void topFavorite(PoiInfoEntity poiInfoEntity, boolean bSetTop) {
        mModel.topFavorite(poiInfoEntity, bSetTop);
    }

    /**
     * 添加收藏夹信息到本地数据库
     * @param entity
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void addFavoriteData(PoiInfoEntity entity, int favoriteType) {
        mModel.addFavoriteData(entity, favoriteType);
    }

    /**
     * 在本地获取家/公司的信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public PoiInfoEntity getFavoriteHomeData(int favoriteType) {
        return mModel.getFavoriteHomeData(favoriteType);
    }

    /**
     * 在本地获取常去地址/普通收藏点的信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public ArrayList<PoiInfoEntity> getFavoriteData(int favoriteType) {
        return mModel.getFavoritePoiData(favoriteType);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param customName  自定义名称 重命名时编辑的字段
     */
    public void modifyFavoriteData(String itemId, String customName) {
        mModel.modifyFavoriteData(itemId, customName);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param topTime 置顶时间
     */
    public void updateFavoriteTopTime(String itemId, long topTime) {
        mModel.updateFavoriteTopTime(itemId, topTime);
    }

    /**
     * 删除 itemId 对应的本地数据
     * @param itemId  收藏点唯一码
     */
    public void deleteFavoriteData(String itemId) {
        mModel.deleteFavoriteData(itemId);
    }

}

