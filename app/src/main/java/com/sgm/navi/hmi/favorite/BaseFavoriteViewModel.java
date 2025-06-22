package com.sgm.navi.hmi.favorite;

import android.app.Application;
import android.os.Bundle;
import android.view.View;

import androidx.annotation.NonNull;
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
    public MutableLiveData<Boolean> mFavoriteListChecked = new MutableLiveData<>(true);
    private final ArrayList<PoiInfoEntity> mStationList = new ArrayList<>();
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

    @Override
    public void onResume() {
        super.onResume();
        if (Boolean.FALSE.equals(mFavoriteListChecked.getValue())) {
            mChargingVisibility.setValue(!mModel.isSGMLogin());
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

    public Action mSendSGMLogin = () -> {
        if (mView != null && mView.getContext() != null) {
            mModel.sendSGMLogin(mView.getContext());
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
        if (Boolean.TRUE.equals(mFavoriteListChecked.getValue())) {
            return;
        }
        mFavoriteListChecked.setValue(true);
        mChargingVisibility.setValue(false);
        mChargingNoDataVisibility.setValue(false);
        mChargingOfflineVisibility.setValue(false);
        mModel.getSimpleFavoriteList();
    };

    public Action mChargingInfoClick = () -> {
        if (Boolean.FALSE.equals(mFavoriteListChecked.getValue())) {
            return;
        }
        mFavoriteListChecked.setValue(false);
        mChargingVisibility.setValue(!mModel.isSGMLogin());
        mTipVisibility.setValue(false);
        mDataVisibility.setValue(false);
        mAddVisibility.setValue(false);
        mModel.queryCollectStation(mView.getActivity());
    };

    /**
     * 获取收藏点列表(普通POI点)
     */
    public void getSimpleFavoriteList() {
        if (Boolean.TRUE.equals(mFavoriteListChecked.getValue())) {
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
        this.mIsHomeCompanyDisplayed.setValue(isHomeCompanyDisplayed);
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
                if (Boolean.TRUE.equals(mFavoriteListChecked.getValue())) {
                    final boolean hasData = !ConvertUtils.isEmpty(list);
                    mTipVisibility.setValue(!hasData);
                    mAddVisibility.setValue(hasData);
                    mDataVisibility.setValue(hasData);
                    if (hasData) {
                        mView.updateFavoriteView(list, type);
                    }
                }
            }else {
                //专属充电站
                if (Boolean.FALSE.equals(mFavoriteListChecked.getValue())) {
                    mDataVisibility.setValue(!list.isEmpty());
                    mChargingNoDataVisibility.setValue(list.isEmpty());
                    mView.updateFavoriteView(list, type);
                }
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
     * 不同车型获取不同数据
     * @return
     */
    public Map<String, Integer> getPopupData() {
        return new HashMap<>(){{
            put("homeOfficeY", -95);
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
        Logger.d(TAG, "station list is " + GsonUtils.toJson(mStationList));
        updateFavoriteView(mStationList, 1);
    }

    /**
     * 网络错误，未获取到数据
     */
    public void notifyConnectStationError() {
        if (Boolean.TRUE.equals(mFavoriteListChecked.getValue())) {
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            mDataVisibility.setValue(false);
            mChargingOfflineVisibility.setValue(true);
        });
    }

    /**
     * parseNetworkResult
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

