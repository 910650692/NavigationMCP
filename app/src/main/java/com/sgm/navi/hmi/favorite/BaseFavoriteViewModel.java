package com.sgm.navi.hmi.favorite;

import android.app.Application;
import android.os.Bundle;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.MutableLiveData;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.hmi.route.RouteFragment;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.BaseViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class BaseFavoriteViewModel extends BaseViewModel<FavoriteFragment, FavoriteModel> {
    public static final String TAG = BaseFavoriteViewModel.class.getName();
    public ObservableField<Boolean> mDataVisibility;
    public ObservableField<Boolean> mTipVisibility;
    public ObservableField<Boolean> mAddVisibility;
    public ObservableField<Boolean> mChargingVisibility;
    public ObservableField<Boolean> mChargingNoDataVisibility;
    public ObservableField<Boolean> mChargingRequestFailedVisibility;
    public ObservableField<Boolean> mChargingOfflineVisibility;
    public ObservableField<Boolean> mIsHomeCompanyDisplayed;
    public ObservableField<Boolean> mIsEVCar;
    public ObservableField<String> mSyncTime;
    public ObservableField<Boolean> mFavoriteListChecked = new ObservableField<>(true);
    private final ArrayList<PoiInfoEntity> mStationList = new ArrayList<>();
    private PoiInfoEntity mHome;
    private PoiInfoEntity mCompany;
    private boolean mIsHome;

    public BaseFavoriteViewModel(final @NonNull Application application) {
        super(application);
        mDataVisibility = new ObservableField<>(false);
        mTipVisibility = new ObservableField<>(true);
        mAddVisibility = new ObservableField<>(false);
        mChargingVisibility = new ObservableField<>(false);
        mChargingNoDataVisibility = new ObservableField<>(false);
        mChargingRequestFailedVisibility = new ObservableField<>(false);
        mChargingOfflineVisibility = new ObservableField<>(false);
        mIsHomeCompanyDisplayed = new ObservableField<>(true);
        mIsEVCar = new ObservableField<>(false);
        mSyncTime = new ObservableField<>();
    }

    @Override
    protected FavoriteModel initModel() {
        return new FavoriteModel();
    }

    public FavoriteFragment getView() {
        return mView;
    }

    @Override
    public void onResume() {
        super.onResume();
        if (Boolean.FALSE.equals(mFavoriteListChecked.get())) {
            mChargingVisibility.set(!mModel.isSGMLogin());
        }
    }

    /**
     * dualChoiceControl
     * @param key
     * @param isTrue
     */
    public void dualChoiceControl(final String key, final boolean isTrue) {
        switch (key) {
            case SettingController.KEY_SETTING_IS_EV_CAR:
                mIsEVCar.set(isTrue);
                break;
            case SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED:
                mIsHomeCompanyDisplayed.set(isTrue);
                break;
            default:
                break;
        }
    }

    public void toHomeFragment() {
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
        bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_HOME_COMPANY_TYPE,
                AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY);
        addFragment(new HomeCompanyFragment(), bundle);
    }

    public void toCompanyFragment() {
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
        bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_HOME_COMPANY_TYPE,
                AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY);
        addFragment(new HomeCompanyFragment(), bundle);
    }

    // 添加家
    public Action mGoSettingHome = () -> {
        if (mHome == null) {
            mModel.getOnlineForecastArrivedData(AutoMapConstant.GuessPositionType.HOME);
        } else {
            startRoute(mHome);
            goHomeOrCompany(AutoMapConstant.HomeCompanyType.HOME);
        }
    };

    // 添加公司
    public Action mGoSettingCompany = () -> {
        if (mCompany == null) {
            mModel.getOnlineForecastArrivedData(AutoMapConstant.GuessPositionType.COMPANY);
        } else {
            startRoute(mCompany);
            goHomeOrCompany(AutoMapConstant.HomeCompanyType.COMPANY);
        }
    };

    public Action mSendSGMLogin = () -> {
        if (mView != null && mView.getContext() != null) {
            mModel.sendSGMLogin(mView.getContext());
        }
    };

    @HookMethod()
    private void goHomeOrCompany(int type){
        String eventName = switch(type){
            case AutoMapConstant.HomeCompanyType.HOME -> BuryConstant.EventName.AMAP_WIDGET_HOME;
            case AutoMapConstant.HomeCompanyType.COMPANY -> BuryConstant.EventName.AMAP_WIDGET_WORK;
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
            ThreadManager.getInstance().execute(() -> RoutePackage.getInstance().requestChangeEnd(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity));

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
            final boolean value = Boolean.FALSE.equals(mIsHomeCompanyDisplayed.get());
            SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, value);
            mIsHomeCompanyDisplayed.set(value);
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
        if (Boolean.TRUE.equals(mFavoriteListChecked.get())) {
            return;
        }
        mFavoriteListChecked.set(true);
        mChargingVisibility.set(false);
        mChargingNoDataVisibility.set(false);
        mChargingOfflineVisibility.set(false);
        mModel.getSimpleFavoriteList();
    };

    public Action mChargingInfoClick = () -> {
        if (Boolean.FALSE.equals(mFavoriteListChecked.get())) {
            return;
        }
        mFavoriteListChecked.set(false);
        refreshCollectStation();
    };

    public void refreshCollectStation() {
        mChargingVisibility.set(!mModel.isSGMLogin());
        mTipVisibility.set(false);
        mDataVisibility.set(false);
        mAddVisibility.set(false);
        mModel.queryCollectStation(mView.getActivity());
    }

    /**
     * 获取收藏点列表(普通POI点)
     */
    public void getSimpleFavoriteList() {
        if (Boolean.TRUE.equals(mFavoriteListChecked.get())) {
            mModel.getSimpleFavoriteList();
        } else {
            if (mModel.isSGMLogin()) {
                mModel.queryCollectStation(mView.getActivity());
            }
        }
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
        this.mIsHomeCompanyDisplayed.set(isHomeCompanyDisplayed);
    }

    /**
     * 获取收藏点列表后更新UI
     *
     * @param list
     * @param type o: favorite list, 1: station list
     */

    public void updateFavoriteView(final ArrayList<PoiInfoEntity> list, final int type) {
        ThreadManager.getInstance().postUi(() -> {
            if (type == 0) {
                //常用收藏夹
                if (Boolean.TRUE.equals(mFavoriteListChecked.get())) {
                    final boolean hasData = !ConvertUtils.isEmpty(list);
                    mTipVisibility.set(!hasData);
                    mAddVisibility.set(hasData);
                    mDataVisibility.set(hasData);
                    mView.updateFavoriteView(list, type);
                }
            } else {
                //专属充电站
                if (Boolean.FALSE.equals(mFavoriteListChecked.get())) {
                    mDataVisibility.set(!list.isEmpty());
                    mChargingNoDataVisibility.set(list.isEmpty());
                    mView.updateFavoriteView(list, type);
                }
            }
        });
    }

    /**
     * 移除收藏点
     *
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
     * 不同车型获取不同数据
     *
     * @return
     */
    public Map<String, Integer> getPopupData() {
        return new HashMap<>(){{
            put("homeOfficeY", -105);
            put("frequentY", -130);
            put("addButtonMargin", 20);
        }};
    }

    /**
     * ND打开新的fragment, 其他车型打开popup
     * @param poiInfo
     * @param view
     */
    public void showRenameDialog(final PoiInfoEntity poiInfo, final View view) {
        mView.showRenameDialog(poiInfo, view);
    }

    /**
     * 更新名称
     * @param name
     */
    public void updateFavoriteName(String name) {
        mView.updateFavoriteName(name);
    }


    /**
     * notifyConnectStationResult
     * @param result
     */
    public void notifyConnectStationResult(BaseRep result) {
        parseStationListResult(result);
        if (Logger.openLog) {
            Logger.d(TAG, "station list is ", mStationList);
        }
        updateFavoriteView(mStationList, 1);
    }

    /**
     * 网络错误，未获取到数据
     */
    public void notifyConnectStationError() {
        if (Boolean.TRUE.equals(mFavoriteListChecked.get())) {
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            mDataVisibility.set(false);
            mChargingOfflineVisibility.set(true);
        });
    }

    /**
     * parseNetworkResult
     *
     * @param result
     */
    private void parseStationListResult(BaseRep result) {
        mStationList.clear();
        try {
            JSONObject jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
            JSONArray jsonArray = jsonObject.getJSONArray("resultList");
            if (jsonArray.length() > 0) {
                for (int i = 0; i < jsonArray.length(); i++) {
                    PoiInfoEntity entity = GsonUtils.fromJson(jsonArray.getString(i), PoiInfoEntity.class);
                    JSONObject object = new JSONObject(String.valueOf(jsonArray.get(i)));
                    GeoPoint point = new GeoPoint();
                    point.setLat(ConvertUtils.str2Double(object.getString("stationLat")));
                    point.setLon(ConvertUtils.str2Double(object.getString("stationLng")));
                    entity.setPoint(point);
                    if (!ConvertUtils.isEmpty(object.getDouble("distance"))) {
                        int distance = ConvertUtils.double2int(object.getDouble("distance") * 1000);
                        final String[] distanceArray = ConvertUtils.formatDistanceArray(getApplication().getBaseContext(), distance);
                        entity.setDistance(distanceArray[0] + distanceArray[1]);
                    }
                    ChargeInfo chargeInfo = GsonUtils.fromJson(jsonArray.getString(i), ChargeInfo.class);
                    chargeInfo.setCurrentElePrice(chargeInfo.getLowPrice())
                        .setFast_free(chargeInfo.getFastChargingFree())
                        .setFast_total(chargeInfo.getFastChargingTotal())
                        .setSlow_free(chargeInfo.getSlowChargingFree())
                        .setSlow_total(chargeInfo.getSlowChargingTotal());
                    List<ChargeInfo> chargeList = new ArrayList<>();

                    chargeList.add(chargeInfo);
                    entity.setChargeInfoList(chargeList)
                        .setName(entity.getStationName())
                        .setAddress(entity.getStationAddress())
                        .setPhone(entity.getStationTel())
                        .setPointTypeCode("011100")
                        .setBusinessTime(entity.getStationBusinessTime());
                    mStationList.add(entity);
                }
            } else {
                Logger.d(TAG, "station list is empty");
            }
        }catch (JSONException e){
            Logger.e(TAG, "error is " + e.getMessage());
        }
    }

}

