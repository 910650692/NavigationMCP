package com.fy.navi.hmi.favorite;

import android.app.Application;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.MutableLiveData;

import com.alibaba.android.arouter.launcher.ARouter;
import com.fy.navi.hmi.route.RouteFragment;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

public class BaseFavoriteViewModel extends BaseViewModel<FavoriteFragment, FavoriteModel> {

    public MutableLiveData<Boolean> mDataVisibility;
    public MutableLiveData<Boolean> mTipVisibility;
    public MutableLiveData<Boolean> mAddVisibility;
    public MutableLiveData<Boolean> mChargingVisibility;
    public MutableLiveData<Boolean> mChargingNoDataVisibility;
    public MutableLiveData<Boolean> mChargingRequestFailedVisibility;
    public MutableLiveData<Boolean> mChargingOfflineVisibility;
    public MutableLiveData<Boolean> mIsHomeCompanyDisplayed;
    public MutableLiveData<Boolean> mIsEVCar;
    private PoiInfoEntity mHome;
    private PoiInfoEntity mCompany;
    private boolean mIsHome;


    public BaseFavoriteViewModel(final @NonNull Application application) {
        super(application);
        mDataVisibility = new MutableLiveData<>(false);
        mTipVisibility = new MutableLiveData<>(true);
        mAddVisibility = new MutableLiveData<>(false);
        mChargingVisibility = new MutableLiveData<>(false);
        mChargingNoDataVisibility = new MutableLiveData<>(false);
        mChargingRequestFailedVisibility = new MutableLiveData<>(false);
        mChargingOfflineVisibility = new MutableLiveData<>(false);
        mIsHomeCompanyDisplayed = new MutableLiveData<>(true);
        mIsEVCar = new MutableLiveData<>(false);
    }

    @Override
    protected FavoriteModel initModel() {
        return new FavoriteModel();
    }

    /**
     * dualChoiceControl
     * @param key
     * @param isTrue
     */
    public void dualChoiceControl(final String key, final boolean isTrue) {
        switch (key) {
            case SettingController.KEY_SETTING_IS_EV_CAR:
                mIsEVCar.setValue(isTrue);
                break;
            case SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED:
                mIsHomeCompanyDisplayed.setValue(isTrue);
                break;
            default:
                break;
        }
    }


    // 添加家
    public Action mGoSettingHome = () -> {
        final Bundle bundle = new Bundle();
        if (mHome == null) {
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
            addFragment(new HomeCompanyFragment(), bundle);
        } else {
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, mHome);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            addFragment(new RouteFragment(), bundle);
        }
    };

    // 添加公司
    public Action mGoSettingCompany = () -> {
        final Bundle bundle = new Bundle();
        if (mCompany == null) {
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
            addFragment(new HomeCompanyFragment(), bundle);
        } else {
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, mCompany);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            addFragment(new RouteFragment(), bundle);
        }
    };

    public Action mShowHomeCompanyDisplayed = () -> {
        final boolean value = Boolean.FALSE.equals(mIsHomeCompanyDisplayed.getValue());
        SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, value);
        mIsHomeCompanyDisplayed.setValue(value);
        mModel.setHomeCompanyDisplay(value);
    };

    public boolean getIsHome() {
        return mIsHome;
    }

    public Action mMoreHome = () -> {
        mIsHome = true;
        mView.showPopupWindow(mView.getHomeOrCompanyEditView(true));
    };

    public Action mMoreCompany = () -> {
        mIsHome = false;
        mView.showPopupWindow(mView.getHomeOrCompanyEditView(false));
    };

    public Action mAddFavorite = () -> {
        final Fragment fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.HOME_COMPANY_FRAGMENT)
                .navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createHomeCompanyFragment(
                AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                AutoMapConstant.SearchType.SEARCH_KEYWORD, AutoMapConstant.HomeCompanyType.COLLECTION));
    };

    /**
     * 手动云服务同步
     */
    public Action mFavoriteSync = () -> {
        mModel.startSync();
    };

    public Action mFavoriteInfoClick = () -> {
        updateFavoriteView(mModel.getFavoritePoiData(0));
    };

    public Action mChargingInfoClick = () -> {
        mTipVisibility.setValue(false);
        mDataVisibility.setValue(false);
        mAddVisibility.setValue(false);
        mChargingVisibility.setValue(true);
    };


    /**
     * 获取收藏点列表
     */
    public void getSimpleFavoriteList() {
        mModel.getSimpleFavoriteList();
    }

    /**
     * getFavoritePoiData
     */
    public void getFavoritePoiData() {
        mModel.getFavoritePoiData();
    }

    /**
     * getHomeInfo
     */
    public void getHomeInfo() {
        mModel.getHomeInfo();
    }

    /**
     * getCompanyInfo
     */
    public void getCompanyInfo() {
        mModel.getCompanyInfo();
    }

    /**
     * updateHomeView
     * @param home
     */
    public void updateHomeView(final PoiInfoEntity home) {
        this.mHome = home;
        mView.updateHomeView(home);
    }

    /**
     * updateCompanyView
     * @param company
     */
    public void updateCompanyView(final PoiInfoEntity company) {
        this.mCompany = company;
        mView.updateCompanyView(company);
    }

    /**
     * getSyncTime
     */
    public void getSyncTime() {
        mModel.getSyncTime();
    }

    /**
     * updateSyncTime
     * @param syncTime
     */
    public void updateSyncTime(final String syncTime) {
        mView.updateSyncTime(syncTime);
    }

    /**
     * updateFavoritePoiData
     * @param list
     */
    public void updateFavoritePoiData(final ArrayList<PoiInfoEntity> list) {
        mView.updateFavoritePoiData(list);
    }

    /**
     * getHomeCompanyInfo
     * @param isHome
     * @return home/office
     */
    public PoiInfoEntity getHomeCompanyInfo(final boolean isHome) {
        if (isHome) {
            return mHome;
        } else {
            return mCompany;
        }
    }

    /**
     * initView
     */
    public void initView() {
        mModel.initView();
    }

    /**
     * setIsHomeCompanyDisplayed
     * @param isHomeCompanyDisplayed
     */
    public void setIsHomeCompanyDisplayed(final boolean isHomeCompanyDisplayed) {
        this.mIsHomeCompanyDisplayed.setValue(isHomeCompanyDisplayed);
    }

    /**
     * 获取收藏点列表后更新UI
     *
     * @param list
     */

    public void updateFavoriteView(final ArrayList<PoiInfoEntity> list) {

        mChargingVisibility.setValue(false);
        mChargingNoDataVisibility.setValue(false);
        mChargingRequestFailedVisibility.setValue(false);
        mChargingOfflineVisibility.setValue(false);

        if (list == null || list.isEmpty()) {
            mTipVisibility.setValue(true);
            mDataVisibility.setValue(false);
            mAddVisibility.setValue(false);
        } else {
            mTipVisibility.setValue(false);
            mDataVisibility.setValue(true);
            mAddVisibility.setValue(true);
            mView.updateFavoriteView(list);
        }
    }

    /**
     * 移除收藏点
     * @param poiInfo
     */
    public void removeFavorite(final PoiInfoEntity poiInfo) {
        mModel.removeFavorite(poiInfo);
    }

    /**
     *  添加POI收藏点
     * @param poiName poi名称
     * @param poiId poiId
     * @param pointX 经度
     * @param pointY 纬度
     * @param type 收藏点类型
     */
    public void addFavorite(final String poiName, final String poiId, final int pointX, final int pointY, final int type) {
        final PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setName(poiName);
        poiInfo.setPid(poiId);
        poiInfo.setPoint(new GeoPoint(pointX, pointY));
        final FavoriteInfo favoriteInfo = new FavoriteInfo()
                .setCommonName(type);
        poiInfo.setFavoriteInfo(favoriteInfo); //  1，家  2，公司  0，普通收藏点;
        mModel.addFavorite(poiInfo);
    }

    /**
     * 收藏点重命名
     *
     * @param customName
     */
    public void modifyFavorite(final String customName) {
        final PoiInfoEntity favoriteInfo = new PoiInfoEntity();
        mModel.modifyFavorite(favoriteInfo, customName);
    }

    /**
     * 收藏点置顶/取消置顶
     *
     * @param poiInfoEntity
     * @param isSetTop       false 取消置顶
     */
    public void topFavorite(final PoiInfoEntity poiInfoEntity, final boolean isSetTop) {
        mModel.topFavorite(poiInfoEntity, isSetTop);
    }

    /**
     * 添加收藏夹信息到本地数据库
     * @param entity
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     */
    public void addFavoriteData(final PoiInfoEntity entity, final int favoriteType) {
        mModel.addFavoriteData(entity, favoriteType);
    }

    /**
     * 在本地获取家/公司的信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return  entity
     */
    public PoiInfoEntity getFavoriteHomeData(final int favoriteType) {
        return mModel.getFavoriteHomeData(favoriteType);
    }

    /**
     * 在本地获取常去地址/普通收藏点的信息
     * @param favoriteType 收藏点类型（1家，2公司，3常去地址，0普通收藏点）
     * @return list
     */
    public ArrayList<PoiInfoEntity> getFavoriteData(final int favoriteType) {
        return mModel.getFavoritePoiData(favoriteType);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param customName  自定义名称 重命名时编辑的字段
     */
    public void modifyFavoriteData(final String itemId, final String customName) {
        mModel.modifyFavoriteData(itemId, customName);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param topTime 置顶时间
     */
    public void updateFavoriteTopTime(final String itemId, final long topTime) {
        mModel.updateFavoriteTopTime(itemId, topTime);
    }

    /**
     * 删除 itemId 对应的本地数据
     * @param itemId  收藏点唯一码
     */
    public void deleteFavoriteData(final String itemId) {
        mModel.deleteFavoriteData(itemId);
    }

}

