package com.fy.navi.hmi.favorite;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.MutableLiveData;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.hmi.route.RouteFragment;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;
import java.util.Objects;

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
    public MutableLiveData<String> mSyncTime;
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
        mSyncTime = new MutableLiveData<>();
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
        if (mHome == null) {
            final Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
            addFragment(new HomeCompanyFragment(), bundle);
        } else {
            startRoute(mHome);
            goHomeOrCompany(AutoMapConstant.HomeCompanyType.HOME);
        }
    };

    // 添加公司
    public Action mGoSettingCompany = () -> {
        if (mCompany == null) {
            final Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
            addFragment(new HomeCompanyFragment(), bundle);
        } else {
            startRoute(mCompany);
            goHomeOrCompany(AutoMapConstant.HomeCompanyType.COMPANY);
        }
    };

    @HookMethod()
    private void goHomeOrCompany(int type){
        String eventName = switch(type){
            case AutoMapConstant.HomeCompanyType.HOME -> BuryConstant.EventName.AMAP_HOME_QUICKACCESS;
            case AutoMapConstant.HomeCompanyType.COMPANY -> BuryConstant.EventName.AMAP_WORK_QUICKACCESS;
            default -> "";
        };
        BuryPointController.getInstance().setEventName(eventName);
    }

    /**
     * 开启算路或者切换终点
     * @param poiInfoEntity
     */
    public void startRoute(final PoiInfoEntity poiInfoEntity) {
        if (Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.SELECT_ROUTE)
            || Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
            RoutePackage.getInstance().requestChangeEnd(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
        } else {
            final Bundle bundle = new Bundle();
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            addFragment(new RouteFragment(), bundle);
        }
    }

    public Action mShowHomeCompanyDisplayed = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_SETTING_HOMEWORKSWITCH)
        public void call() {
            final boolean value = Boolean.FALSE.equals(mIsHomeCompanyDisplayed.getValue());
            SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, value);
            mIsHomeCompanyDisplayed.setValue(value);
            mModel.setHomeCompanyDisplay(value);

            BuryProperty property = new BuryProperty.Builder().setParams(BuryConstant.ProperType.BURY_KEY_SETTING_CONTENT, value ? BuryConstant.Number.SECOND : BuryConstant.Number.ONE).build();
            BuryPointController.getInstance().setBuryProps(property);
        }
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
        mModel.getSimpleFavoriteList();
    };

    public Action mChargingInfoClick = () -> {
        mTipVisibility.setValue(false);
        mDataVisibility.setValue(false);
        mAddVisibility.setValue(false);
        mChargingVisibility.setValue(true);
    };

    /**
     * 获取收藏点列表(普通POI点)
     */
    public void getSimpleFavoriteList() {
        mModel.getSimpleFavoriteList();
    }

    /**
     * getHomeInfo
     */
    public void getHomeFavoriteInfo() {
        mModel.getHomeFavoriteInfo();
    }

    /**
     * getCompanyInfo
     */
    public void getCompanyFavoriteInfo() {
        mModel.getCompanyFavoriteInfo();
    }

    /**
     * getAddressInfo
     */
    public ArrayList<PoiInfoEntity> getFavoriteAddressInfo() {
        return mModel.getFavoriteAddressInfo();
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
        ThreadManager.getInstance().postUi(() -> {
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
        });
    }

    /**
     * 移除收藏点
     * @param poiInfo
     */
    public void removeFavorite(final PoiInfoEntity poiInfo) {
        mModel.removeFavorite(poiInfo);
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

