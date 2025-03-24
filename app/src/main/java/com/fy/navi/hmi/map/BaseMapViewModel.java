package com.fy.navi.hmi.map;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.annotation.SuppressLint;
import android.app.Application;
import android.os.Bundle;
import android.os.Looper;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.databinding.ObservableBoolean;
import androidx.databinding.ObservableField;
import androidx.databinding.ObservableInt;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.favorite.FavoriteHelper;
import com.fy.navi.hmi.favorite.HomeCompanyFragment;
import com.fy.navi.hmi.favorite.MapPointSearchFragment;
import com.fy.navi.hmi.limit.LimitDriveFragment;
import com.fy.navi.hmi.mapdata.MapDataFragment;
import com.fy.navi.hmi.navi.NaviGuidanceFragment;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.hmi.route.RouteFragment;
import com.fy.navi.hmi.search.mainsearch.MainSearchFragment;
import com.fy.navi.hmi.search.searchresult.SearchResultFragment;
import com.fy.navi.hmi.setting.SettingFragment;
import com.fy.navi.hmi.traffic.TrafficEventFragment;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.AutoMapConstant.PoiType;
import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.aos.RestrictedAreaDetail;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.message.MessageCenterInfo;
import com.fy.navi.service.define.message.MessageCenterType;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.base.StackManager;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/1/25
 */
public class BaseMapViewModel extends BaseViewModel<MapActivity, MapModel> {
    private static final String TAG = "BaseMapViewModel";
    public ObservableBoolean backToCcPVisibility;
    public ObservableBoolean mainBTNVisibility;
    public ObservableBoolean bottomNaviVisibility;
    public ObservableBoolean backToParkingVisibility;
    public ObservableBoolean messageCenterVisible;
    public ObservableField<MessageCenterInfo> messageCenterEntity;
    public ObservableField<String> messageCenterOperate;
    public ObservableField<String> messageCenterTitle;
    public ObservableField<String> messageCenterContent;
    public ObservableBoolean messageCenterContentVisibility;
    public ObservableField<Boolean> naviHomeVisibility;
    public ObservableInt carModeImgId;
    @Nullable
    private RestrictedArea restrictedArea;
    @Nullable
    private RouteRestrictionParam routeRestrictionParam;
    public ObservableField<Boolean> limitDriverVisibility;
    public ObservableField<String> limitDriverTitle;
    public ObservableField<Boolean> cruiseVisibility;
    public ObservableField<Boolean> muteVisibility;


    public BaseMapViewModel(@NonNull Application application) {
        super(application);
        backToCcPVisibility = new ObservableBoolean(false);
        mainBTNVisibility = new ObservableBoolean(true);
        naviHomeVisibility = new ObservableField<>(false);
        limitDriverVisibility = new ObservableField<>(false);
        limitDriverTitle = new ObservableField<>("");
        carModeImgId = new ObservableInt(R.drawable.img_car_mode_2d_north);
        cruiseVisibility = new ObservableField<>(false);
        muteVisibility = new ObservableField<>(true);
        bottomNaviVisibility = new ObservableBoolean(true);
        backToParkingVisibility = new ObservableBoolean(false);
        messageCenterVisible = new ObservableBoolean(false);
        messageCenterEntity = new ObservableField<>();
        messageCenterOperate = new ObservableField<>("");
        messageCenterTitle = new ObservableField<>("");
        messageCenterContent = new ObservableField<>("");
        messageCenterContentVisibility = new ObservableBoolean(false);
    }

    @Override
    protected MapModel initModel() {
        return new MapModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mModel.checkContinueNavi(mView);
    }

    @Override
    public void onResume() {
        super.onResume();
        mModel.checkAuthorizationExpired();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public Action intercept = () -> {
        // 拦截控件点击事件
        mModel.stopCruise();
    };

    //回自车位逻辑
    public Action backToSelfParking = () -> {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapTypeId.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
    };

    // 播报和静音切换
    public Action muteOrUnMute = () -> {
        mModel.setCruiseVoice(!muteVisibility.get());
    };

    public Action openSearchFragment = () -> {
        addFragment(new MainSearchFragment(), null);
        mModel.stopCruise();
    };

    public Action openHomeFragment = () -> {
        try {
            PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_HOME);
            if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                //直接进入导航到家
                SearchPackage.getInstance().clearLabelMark();
                startRoute(poiInfoEntity);
                return;
            }
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
            addFragment(new HomeCompanyFragment(), bundle);
            mModel.stopCruise();
        } catch (Exception e) {
            Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
        }
    };

    public Action openCompanyFragment = () -> {
        try {
            PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_COMPANY);
            if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                //直接进入导航到公司
                SearchPackage.getInstance().clearLabelMark();
                startRoute(poiInfoEntity);
                return;
            }
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
            addFragment(new HomeCompanyFragment(), bundle);
            mModel.stopCruise();
        } catch (Exception e) {
            Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
        }
    };

    public Action searchForChargeStation = () -> {
        try {
            final Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            final int powerType = powerType();
            // 油车
            if (powerType == 0) {
                bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, "加油站");
            } else {
                bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, "充电站");
            }
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, null);
            addFragment(new SearchResultFragment(), bundle);

        } catch (Exception e) {
            Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
        }
    };

    public void searchForExtraKeyword(String keyword) {
        try {
            Logger.d(SEARCH_HMI_TAG, "searchExtraKeyword: " + keyword);
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, null);
            addFragment(new SearchResultFragment(), bundle);
        } catch (Exception e) {
            Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
        }
    }


    public Action openSettingFragment = () -> {
        messageCenterVisible.set(false);
        addFragment(new SettingFragment(), null);
    };

    public Action carHeader = () -> {
        mModel.switchMapMode();
    };

    public Action messageCenterGone = () -> {
        messageCenterVisible.set(false);
    };

    public Action openLimitDetailFragment = () -> {
        if (needInterceptor()) return;
        Bundle bundle = new Bundle();
        bundle.putSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_DRIVER, routeRestrictionParam);
        addFragment(new LimitDriveFragment(), bundle);
    };

    public Action messageCenterOperateClick = () -> {
         messageCenterGone.call();
         final MessageCenterInfo messageCenterInfo = messageCenterEntity.get();
         if(messageCenterInfo!=null){
             if(messageCenterInfo.getMsgType() == MessageCenterType.ROAD_LIMIT){
                 openLimitDetailFragment.call();
             }else if(messageCenterInfo.getMsgType() == MessageCenterType.MAP_UPDATE_15 ||
                     messageCenterInfo.getMsgType() == MessageCenterType.MAP_UPDATE_45){
                 final Bundle bundle = new Bundle();
                 bundle.putBoolean("isCheck",true);
                 addFragment(new MapDataFragment(), bundle);
             }
         }
    };


    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        mModel.loadMapView(mapSurfaceView);
    }

    public void startListenMsg(){
        mModel.startListenMsg();
    }

    public void getCurrentCityLimit() {
        mModel.getCurrentCityLimit();
    }

    public void setMapCenterInScreen(int frameLayoutWidth) {
        mModel.setMapCenterInScreen(frameLayoutWidth);
        mainBTNVisibility.set(false);
        bottomNaviVisibility.set(false);
        backToParkingVisibility.set(false);
    }

    public void resetMapCenterInScreen() {
        mModel.resetMapCenterInScreen();
        mainBTNVisibility.set(true);
        bottomNaviVisibility.set(true);
    }

    public void setBacktoCCP(boolean isTouchStatus) {
        backToCcPVisibility.set(isTouchStatus);
    }

    public void showOrHideSelfParkingView(boolean visible){
        backToParkingVisibility.set(visible);
    }

    /**
     * UE单位是公里
     *
     * @param scale 单位是米
     */
    public void updateOnMapScaleChanged(int scale) {
        String format;
        int kilometer;
        if (scale / 1000 > 0) {
            format = "%d公里";
            kilometer = scale / 1000;
        } else {
            format = "%d米";
            kilometer = scale;
        }
        mView.updateOnMapScaleChanged(String.format(format, kilometer));
    }

    public void toPoiDetailFragment(PoiInfoEntity entity) {
        if (needInterceptor() && FavoriteHelper.getInstance().getHomeCompanyType() != 4) {
            return;
        }
        if (FavoriteHelper.getInstance().getHomeCompanyType() != -1) {
            //如果正在执行地图选点流程，点击item后拉起MapPointSearchFragment进行搜索
//            Fragment fragment = StackManager.getInstance().getCurrentFragment(mScreenId);
//            if (fragment instanceof MapPointSearchFragment) {
//                ((MapPointSearchFragment) fragment).doSearch(entity);
            addFragment(new MapPointSearchFragment(), MapPointSearchFragment.getBundle(FavoriteHelper.getInstance().getHomeCompanyType(), entity));
//            }
            return;
        }
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, entity);
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, PoiType.POI_MAP_CLICK);
        PoiDetailsFragment fragment = new PoiDetailsFragment();
        addPoiDetailsFragment(fragment, bundle);
    }

    public void toSearchResultFragment(String keyword) {
        Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, null);
        addFragment(new SearchResultFragment(), args);
    }

    public void toSearchAroundFragment(String keyword, PoiInfoEntity endPoint) {
        Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.AROUND_SEARCH);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, endPoint);
        addFragment(new SearchResultFragment(), args);
    }

    public void toRouteFragment(RouteSpeechRequestParam param) {
        Bundle args = new Bundle();
        args.putSerializable("speech_open_route", param);
        addFragment(new RouteFragment(), args);
    }

    public void openTrafficDetailFragment(PoiInfoEntity entity) {
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.TrafficEventBundleKey.BUNDLE_KEY_ENTITY, entity);
        TrafficEventFragment trafficEventFragment;
        BaseFragment fragment =  StackManager.getInstance().getCurrentFragment(mScreenId);
        if (fragment != null && fragment instanceof TrafficEventFragment) {
            trafficEventFragment = (TrafficEventFragment) fragment;
            trafficEventFragment.setArguments(bundle);
            trafficEventFragment.onInitData();
        } else {
            trafficEventFragment = new TrafficEventFragment();
            addFragment(trafficEventFragment, bundle);
        }
    }

    /**
     * 跳转到设置公司-家界面.
     *
     * @param type int，1-HOME，2-COMPANY.
     * @param keyword 搜索关键字.
     */
    public void toHomeCompanyFragment(int type, String keyword) {
        Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT,
                AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE,
                AutoMapConstant.SearchType.SEARCH_KEYWORD);
        args.putString(AutoMapConstant.VoiceKeyWord.BUNDLE_VOICE_KEY_WORD, keyword);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, type);
        addFragment(new HomeCompanyFragment(), args);
    }

    public void updateUiStyle(MapTypeId mapTypeId, int uiMode) {
        mModel.updateUiStyle(mapTypeId, uiMode);
    }

    // TODO 到了下班时间收到回家推送消息，显示回家UI---待推送功能实现
    public void showNaviHome() {
        naviHomeVisibility.set(true);
    }

    // 保存最后一次定位的位置信息
    public void saveLastLocationInfo() {
        mModel.saveLastLocationInfo();
    }

    public void setLimitDriverVisibility(boolean visibility) {
        limitDriverVisibility.set(visibility);
    }

    public void updateLimitInfo(@Nullable RouteRestrictionParam param) {
        if (param == null) {
            return;
        }
        this.routeRestrictionParam = param;
        this.restrictedArea = param.getMRestrictedArea();
        // 导航中或者算路中不显示
        boolean statusVis = mModel.getNaviStatus() == NaviStatus.NaviStatusType.NO_STATUS || mModel.getNaviStatus() == NaviStatus.NaviStatusType.CRUISE;
        Logger.d(TAG, "statusVis:" + statusVis, "restrictedArea:" + (restrictedArea != null));

        boolean flag = false;
        for (RestrictedAreaDetail restrictedAreaDetail : this.restrictedArea.getMRestrictedAreaDetails().get(0)) {
            if (restrictedAreaDetail.getMEffect() == 1) {
                flag = true;
                break;
            }
        }
        if (flag) {
            limitDriverTitle.set(getApplication().getString(R.string.limit_today_drive));
        } else {
            limitDriverTitle.set(getApplication().getString(R.string.limit_drive));
        }
        limitDriverVisibility.set(restrictedArea != null && statusVis);

        if(restrictedArea != null && statusVis){
            //首页消息的显示逻辑  发送package消息
            final boolean showSameDayLimit = mModel.showSameDayLimit();
            if(showSameDayLimit){
                Logger.i("showSameDayLimit","showSameDayLimit"+statusVis);
                mModel.managerMessage(MessageCenterType.ROAD_LIMIT,"");
            }
        }
    }

    /**
     * 离线地图是否15天未更新
     */
    public void offlineMap15Day(){
        if(mModel.offlineMap15Day()){
            mModel.managerMessage(MessageCenterType.MAP_UPDATE_15,"");
        }
    }

    /**
     * 离线地图是否45天未更新
     */
    public void offlineMap45Day(){
        if(mModel.offlineMap45Day()){
            mModel.managerMessage(MessageCenterType.MAP_UPDATE_45,"");
        }
    }

    /**
     * @param messageCenterInfo 数据
     */
    public void onMessageInfoNotifyCallback(final MessageCenterInfo messageCenterInfo){
        messageCenterEntity.set(messageCenterInfo);
        messageCenterVisible.set(true);
        messageCenterOperate.set(messageCenterInfo.getMsgOperate());
        messageCenterTitle.set(messageCenterInfo.getMsgTitle());
        messageCenterContent.set(messageCenterInfo.getMsgContent());
        if(messageCenterInfo.getMsgType() == MessageCenterType.ROAD_LIMIT){
            messageCenterContentVisibility.set(false);
        }else if(messageCenterInfo.getMsgType() == MessageCenterType.MAP_UPDATE_15){
            messageCenterContentVisibility.set(false);
        }else if(messageCenterInfo.getMsgType() == MessageCenterType.MAP_UPDATE_45){
            messageCenterContentVisibility.set(true);
        }
    }

    /**
     * 移除消息
     */
    public void onMessageInfoRemoveCallback(){
        messageCenterVisible.set(false);
    }

    // 如果处于特定状态不允许POI响应点击事件, 现在有两种“巡航”和“无状态”
    private boolean needInterceptor() {
        String currentStatus = mModel.getNaviStatus();
        return currentStatus == NaviStatus.NaviStatusType.NAVING
                || currentStatus == NaviStatus.NaviStatusType.LIGHT_NAVING
                || currentStatus == NaviStatus.NaviStatusType.SELECT_ROUTE
                || currentStatus == NaviStatus.NaviStatusType.ROUTING;
    }

    public void onNaviStatusChange() {
        updateLimitInfo(routeRestrictionParam);
    }

    //传递搜索参数
    public void setExtraKeyword(String keyword) {
        if (null == keyword || keyword.isEmpty()) {
            return;
        }
        mModel.setSearchKeyword(keyword);
    }

    /**
     * 获取收藏点（家、公司、常用地址、收藏）
     *
     * @param poiType
     * @return
     */
    @SuppressLint("SwitchIntDef")
    private PoiInfoEntity getFavoritePoiInfo(@PoiType int poiType) {
        return switch (poiType) {
            case PoiType.POI_HOME ->
                    BehaviorPackage.getInstance().getFavoriteHomeData(AutoMapConstant.HomeCompanyType.HOME); //暂时取的都是本地的
            case PoiType.POI_COMPANY ->
                    BehaviorPackage.getInstance().getFavoriteHomeData(AutoMapConstant.HomeCompanyType.COMPANY);
            default -> null;
        };
    }

    /***触发算路***/
    public void startRoute(PoiInfoEntity poiInfoEntity) {
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
        addFragment(new RouteFragment(), bundle);
    }

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }

    public void showToast(@StringRes int res) {
        if (Looper.getMainLooper() != Looper.myLooper()) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(mView.getString(res));
            });
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(mView.getString(res));
        }
    }

    // 更新巡航态下的车道信息
    public void updateCruiseLanInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        mView.updateCruiseLanInfo(isShowLane, laneInfoEntity);
    }

    public void showOrHiddenCruise(boolean isShow) {
        cruiseVisibility.set(isShow);
        bottomNaviVisibility.set(!isShow && mainBTNVisibility.get());
    }

    public void setCruiseMuteOrUnMute(boolean isOpen) {
        muteVisibility.set(isOpen);
        mView.cruiseMuteOrUnMute(isOpen);
    }

    public void stopCruise() {
        mModel.stopCruise();
    }

    public void openRoute(PoiInfoEntity endPoint) {
        if (ConvertUtils.isEmpty(endPoint)) {
            return;
        }
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, endPoint);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
        addFragment(new RouteFragment(), bundle);
    }

    public void startNaviForRouteOver() {
        if (NaviStatus.NaviStatusType.SELECT_ROUTE == NaviStatusPackage.getInstance().getCurrentNaviStatus()) {
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, AutoMapConstant.NaviType.NAVI_GPS);
            addFragment(new NaviGuidanceFragment(), bundle);
        }
    }

    public void openCollectFragment() {
        addFragment(new SettingFragment(), null);
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        return mModel.powerType();
    }
}
