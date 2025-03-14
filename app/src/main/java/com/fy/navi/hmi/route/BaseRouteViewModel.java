package com.fy.navi.hmi.route;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.navi.NaviGuidanceFragment;
import com.fy.navi.hmi.search.alongway.MainAlongWaySearchFragment;
import com.fy.navi.hmi.setting.SettingFragment;
import com.fy.navi.scene.api.route.ISceneRouteDetailsSelectCallBack;
import com.fy.navi.scene.api.route.ISceneRouteGasStationChargeSelectCallBack;
import com.fy.navi.scene.api.route.ISceneRouteGasStationWeatherServiceSelectCallBack;
import com.fy.navi.scene.api.route.ISceneRoutePreferenceCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSearchChargeRefreshItemCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSearchRefreshItemCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSelectCallBack;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.search.SearchConfirmDialog;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RouteRestirctionID;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.route.RouteWeatherInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ServiceAreaInfo;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ScheduledFuture;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class BaseRouteViewModel extends BaseViewModel<RouteFragment, RouteModel> implements ISceneRoutePreferenceCallBack
        , ISceneRouteSelectCallBack
        , ISceneRouteDetailsSelectCallBack
        , ISceneRouteSearchRefreshItemCallBack
        , ISceneRouteGasStationWeatherServiceSelectCallBack
        , ISceneRouteGasStationChargeSelectCallBack
        , ISceneRouteSearchChargeRefreshItemCallBack
        , View.OnTouchListener {

    private static final String TAG = BaseRouteViewModel.class.getSimpleName();
    public ObservableField<Integer> tabVisibility;
    public ObservableField<Boolean> elecCheckBoxVisibility;
    public ObservableField<Boolean> alongTabSearchVisibility;
    /**主页面-详情页**/
    public ObservableField<Integer> includePageVisibility;
    /**主页面-偏好页**/
    public ObservableField<Boolean> routePreferenceVisibility;
    /**主页面-偏好ICON**/
    public ObservableField<Drawable> routePreferenceDrawableVisibility;
    /**主页面-标题-终点名称~我的位置**/
    public ObservableField<String> title;
    /**主页面-限行文本**/
    public ObservableField<String> restriction;
    /**主页面-限行文本颜色**/
    public ObservableField<Integer> restrictionTextColor;
    /**主页面-限行背景**/
    public ObservableField<Drawable> restrictionBackground;
    /**主页面-限行可见**/
    public ObservableField<Boolean> restrictionVisibility;
    /**主页面-限行左侧车标可见**/
    public ObservableField<Boolean> restrictionCarVisibility;
    /**主页面-限行右侧back可见**/
    public ObservableField<Boolean> restrictionRightBackVisibility;
    /**主页面-途经点—终点名称**/
    public ObservableField<String> endName;
    /**主页面-偏好名称**/
    public ObservableField<String> preferText;
    /**主页面-开始导航文本**/
    public ObservableField<String> countDownHint;
    /**主页面-途径点-缩小描述**/
    public ObservableField<String> paramDes;
    /**主页面-有无途经点**/
    public ObservableField<Boolean> viaPoiListAllVisibility;
    /**主页面-途经点是否展开**/
    public ObservableField<Boolean> routeDetailsVisibility;
    /**详情页面-避开按钮背景**/
    public ObservableField<Drawable> avoidBackground;
    /**详情页面-避开按钮是否可点击**/
    public ObservableField<Boolean> avoidClickable;
    /**详情页面-避开按钮背景**/
    public ObservableField<Boolean> viaPoiListVisibility;
    /**天气详情页面**/
    public ObservableField<Drawable> weatherDrawable;
    public ObservableField<String> weatherName;
    public ObservableField<String> weatherCityName;
    public ObservableField<String> weatherTimeAndDistance;
    public ObservableField<String> weatherDiscription;
    public ObservableField<String> weatherUpdateTime;
    /**POI详情页面**/
    public ObservableField<Boolean> routeSearchStatusVisibility;
    public ObservableField<String> routeSearchStatus;
    public ObservableField<String> routeSearchName;
    public ObservableField<String> routeSearchAddress;
    public ObservableField<String> routeSearchTimeAndDistance;
    public ObservableField<String> routeSearchElec;
    public ObservableField<Integer> routeSearchTypeVisibility;
    public ObservableField<String> routeSearchDeailsAddRemoveVia;
    /**POI充电站页面**/
    public ObservableField<String> routeChargeTotalMileage;
    public ObservableField<Boolean> routeProgressChargeVisibility;
    public ObservableField<Boolean> routeProgressChargeExhaustVisibility;
    public ObservableField<Integer> routeProgressChargeExhaust;
    private ScheduledFuture scheduledFuture;
    private int times = NumberUtils.NUM_9;

    private int mRestrictionStatus = 0;
    private String currentPlateNumber;
    private String currentavoidLimit;
    private String currentPreferences;
    private String currentEnergy;
    private String searchKeyWord = ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge);

    private List<String> currentPageHistory = new ArrayList<>();
    private PoiInfoEntity mDetailsEntry;
    private PoiInfoEntity mDetailsResustEntry;
    private int mSeartype;
    private List<RouteParam> mGasChargeAlongList;

    public BaseRouteViewModel(@NonNull Application application) {
        super(application);
        currentPageHistory.add("0");
        includePageVisibility = new ObservableField<>(getCurrentPageUI());
        tabVisibility = new ObservableField<>(0);
        elecCheckBoxVisibility = new ObservableField<>(false);
        alongTabSearchVisibility = new ObservableField<>(true);
        routePreferenceVisibility = new ObservableField<>(false);
        routePreferenceDrawableVisibility = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
        countDownHint = new ObservableField<>(ResourceUtils.Companion.getInstance().getString(R.string.route_start_navi) + "(" + NumberUtils.NUM_9 + "s)");
        restriction = new ObservableField<>("");
        restrictionBackground = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.color.text_route_restriction_error));
        restrictionTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().getColor(R.color.text_route_restriction_text_error));
        routePreferenceDrawableVisibility = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));

        restrictionVisibility = new ObservableField<>(false);
        restrictionCarVisibility = new ObservableField<>(false);
        restrictionRightBackVisibility = new ObservableField<>(false);
        paramDes = new ObservableField<>("");
        avoidBackground = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_route_details_defult_label));
        avoidClickable = new ObservableField<>(false);
        title = new ObservableField<>();
        endName = new ObservableField<>();
        preferText = new ObservableField<>(ResourceUtils.Companion.getInstance().getString(R.string.route_change_preference));
        routeDetailsVisibility = new ObservableField<>(true);
        viaPoiListVisibility = new ObservableField<>(false);
        viaPoiListAllVisibility = new ObservableField<>(false);

        weatherDrawable = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_weather_clear));
        weatherName = new ObservableField<>("");
        weatherCityName = new ObservableField<>("");
        weatherTimeAndDistance = new ObservableField<>("");
        weatherDiscription = new ObservableField<>("");
        weatherUpdateTime = new ObservableField<>("");

        routeSearchStatusVisibility = new ObservableField<>(false);
        routeSearchStatus = new ObservableField<>("");
        routeSearchName = new ObservableField<>("");
        routeSearchAddress = new ObservableField<>("");
        routeSearchTimeAndDistance = new ObservableField<>("");
        routeSearchElec = new ObservableField<>("");
        routeSearchTypeVisibility = new ObservableField<>(0);
        routeSearchDeailsAddRemoveVia = new ObservableField<>("");
        routeChargeTotalMileage = new ObservableField<>("");
        routeProgressChargeVisibility = new ObservableField<>(false);
        routeProgressChargeExhaustVisibility = new ObservableField<>(true);
        routeProgressChargeExhaust = new ObservableField<>(0);
    }
    private Integer getCurrentPageUI() {
        Logger.i(TAG, "getCurrentPageUI: ==>");
        if (currentPageHistory.isEmpty()) return 0;
        Logger.i(TAG, "getCurrentPageUI: " + Integer.parseInt(currentPageHistory.get(currentPageHistory.size() -1)));
        return Integer.parseInt(currentPageHistory.get(currentPageHistory.size() -1));
    }
    private void removeCurrentPageUI(String includeIndex){
        if (currentPageHistory.isEmpty()) return;
        if (currentPageHistory.contains(includeIndex)) {
            currentPageHistory.remove(includeIndex);
        }
    }
    @Override
    protected RouteModel initModel() {
        return new RouteModel();
    }
    //算路页面
    public Action closeRoute = () -> {
        Logger.i(TAG, "closeRoute: ==>");
        if (Boolean.TRUE.equals(routePreferenceVisibility.get())) {
            routePreferenceVisibility.set(false);
            routePreferenceDrawableVisibility.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
            return;
        }
        cancelTimer();
        mModel.clearRouteLine();
        mModel.clearRestrictionView();
        mView.hideTrip();
        closeFragment(true);
    };
    public Action restrictionClick = () -> {
        switch (mRestrictionStatus) {
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENOPLATE:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENOTOPEN:
                cancelTimer();
                addFragment(new SettingFragment(), null);
                break;
            default:
                mModel.showRestrictionFragment();
                break;
        }
    };
    public Action preferenceClick = () -> {
        cancelTimer();
        routePreferenceVisibility.set(Boolean.FALSE.equals(routePreferenceVisibility.get()));
        routePreferenceDrawableVisibility.set(Boolean.TRUE.equals(routePreferenceVisibility.get()) ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_up) : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
    };

    public Action alongWaySearch = () -> {
        cancelTimer();
        addFragment(new MainAlongWaySearchFragment(), null);
    };

    public Action openCloseVia = () -> {
        cancelTimer();
        routePreferenceVisibility.set(false);
        viaPoiListAllVisibility.set(Boolean.FALSE.equals(viaPoiListAllVisibility.get()));
    };

    public Action routeEnergy = () -> {
        cancelTimer();
        mView.setEnergyChecked();
    };

    public Action startNaviClick = () -> {
        cancelTimer();
        mModel.clearSearchLabel();
        mModel.clearWeatherView();
        mView.hideTrip();
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, AutoMapConstant.NaviType.NAVI_GPS);
        addFragment(new NaviGuidanceFragment(), bundle);
    };

    public Action refreshRoute = () -> {
        cancelTimer();
        requestRoute(null, -1, RouteWayID.ROUTE_WAY_REFRESH);
    };

    //路线详情页面
    public Action closeDetailsRoute = () -> {
        if (Boolean.FALSE.equals(routeDetailsVisibility.get())) {
            routeDetailsVisibility.set(true);
            mView.setAvoidStatusUI(false);
            return;
        }
        currentPageHistory.remove(currentPageHistory.size() -1);
        includePageVisibility.set(getCurrentPageUI());
    };
    public Action startSimNaviClick = () -> {
        mModel.clearSearchLabel();
        mModel.clearWeatherView();
        mView.hideTrip();
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, AutoMapConstant.NaviType.NAVI_SIMULATE);
        addFragment(new NaviGuidanceFragment(), bundle);
    };
    public Action avoidRoad = () -> {
        routeDetailsVisibility.set(false);
        mView.setAvoidStatusUI(true);
    };
    public Action avoid = () -> mView.startAvoidRoad();
    //天气详情
    public Action closeWeatherDetails = this::hideWeatherDeatilsUI;
    public Action weatherAviod = this::hideWeatherDeatilsUI;
    public Action serviceRefresh = () -> ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.route_no_function));
    public Action closeServicelist = () -> {
        mView.clearSceneTabUI();
        isChargingSelect = false;
        mView.clearSceneTabUI(false);
        hideRouteSearchListUI();
        mModel.clearSearchLabel();
    };

    public Action closeChargelist = () -> {
        mView.clearSceneTabUI();
        isChargingSelect = false;
        mView.clearSceneTabUI(false);
        hideRouteSearchChargeListUI();
        mModel.clearSearchLabel();
    };
    public Action chargeRefresh = () -> ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.route_no_function));
    //POI
    public Action closeRouteSearchDetail = this::hideRouteSearchDetailsUI;
    public Action routeSearchDetailsAddRemove = () -> {
        if (ConvertUtils.isEmpty(mDetailsEntry)) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.route_status_issue));
            return;
        }
        if (mSeartype == 0) {
            if (isBelongSamePoi(mGasChargeAlongList, mDetailsEntry)) {
                //删除途经点
                mModel.gasChargeRemoveMode(mDetailsEntry);
            } else {
                //添加途径点
                mModel.gasChargeAddMode(mDetailsEntry);
            }
            hideRouteSearchDetailsUI();
        } else {
            if (mModel.isBelongRouteParam(mDetailsEntry)) {
                //删除途经点
                mModel.deleteViaParamMode(mDetailsEntry);
            } else {
                //添加途径点
                mModel.addViaList(mDetailsEntry);
            }
        }
    };
    public Action routeSearchPhone = () -> {
        if (!ConvertUtils.isEmpty(mDetailsResustEntry) && !ConvertUtils.isEmpty(mDetailsResustEntry.getPhone())) {
            String phone = mDetailsResustEntry.getPhone();
            List<String> phoneString = new ArrayList<>();
            if (phone.contains(";")) {
                String[] split = phone.split(";");
                phoneString.addAll(Arrays.asList(split));
            } else {
                phoneString.add(phone);
            }
            if (!ConvertUtils.isEmpty(phoneString) && !ConvertUtils.isEmpty(phoneString.get(0))) {
                Logger.d(SEARCH_HMI_TAG, "call phone: " + phoneString.get(0));
                new SearchConfirmDialog.Build(mView.getContext())
                        .setDialogObserver(new IBaseDialogClickListener() {
                            @Override
                            public void onCommitClick() {
                                //拨打电话
                                Intent intent = new Intent();
                                intent.setAction(Intent.ACTION_CALL);
                                intent.setData(Uri.parse("tel:" + phoneString.get(0)));
                                Context context = mView.getContext();
                                context.startActivity(intent);
                            }

                            @Override
                            public void onCancelClick() {

                            }
                        })
                        .setContent(mView.getContext().getString(com.fy.navi.scene.R.string.text_dial_phone_content, phoneString.get(0)))
                        .setConfirmTitle(mView.getContext().getString(com.fy.navi.scene.R.string.text_dial))
                        .build().show();

            } else {
                Logger.d(TAG, "call phone is null ");
            }
        }
    };
    public Action routeSearchAround = () -> {};
    public Action routeSearchFavorite = () -> {};
    public Action chargeSureClick = () -> {
        mModel.startAllRequest();
    };
    public Action chargeCancelClick = () -> {
        mView.clearSceneTabUI();
        isChargingSelect = false;
        mView.clearSceneTabUI(false);
        hideRouteSearchChargeListUI();
        mModel.clearSearchLabel();
    };

    public void initTimer() {
        scheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (times == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(() -> {
                    cancelTimer();
                    mModel.clearSearchLabel();
                    mModel.clearWeatherView();
                    mView.hideTrip();
                    Bundle bundle = new Bundle();
                    bundle.putInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, AutoMapConstant.NaviType.NAVI_GPS);
                    addFragment(new NaviGuidanceFragment(), bundle);
                });
            } else {
                String hint = ResourceUtils.Companion.getInstance().getString(R.string.route_start_navi) + "(" + times + "s)";
                countDownHint.set(hint);
            }
            times--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }
    public void cancelTimer() {
        if (!ConvertUtils.isEmpty(scheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(scheduledFuture);
            countDownHint.set(ResourceUtils.Companion.getInstance().getString(R.string.route_start_navi));
            times = NumberUtils.NUM_9;
            scheduledFuture = null;
        }
    }
    public void requestRoute(PoiInfoEntity poiInfoEntity, int poiType, int routeWay) {
        mModel.requestRouteMode(poiInfoEntity, poiType, routeWay);
    }

    public void requestRouteRestoration(RouteMsgPushInfo routeMsgPushInfo) {
        mModel.requestRouteRestoration(routeMsgPushInfo);
    }

    public void requestRouteFromSpeech(RouteSpeechRequestParam param) {
        mModel.requestRouteFromSpeech(param);
    }
    public void showNomalRouteUI() {
        mView.clearSceneTabUI();
        isChargingSelect = false;
        mView.clearSceneTabUI(false);
        currentPageHistory.clear();
        currentPageHistory.add("0");
        includePageVisibility.set(getCurrentPageUI());
        routePreferenceVisibility.set(false);
        routePreferenceDrawableVisibility.set(Boolean.TRUE.equals(routePreferenceVisibility.get()) ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_up) : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
        routeDetailsVisibility.set(true);
        avoidBackground.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_route_details_defult_label));
        avoidClickable.set(false);
        mView.setAvoidStatusUI(false);
    }
    public void showProgressUI() {
        ThreadManager.getInstance().postUi(() -> {
            mView.showProgressUI();
            //重算路关闭Route/Guidance上面所有页面
            closeAllFragmentUpRoute();
        });
    }
    public void hideProgressUI() {
        ThreadManager.getInstance().postUi(() -> mView.hideProgressUI());
    }
    public void showSearchProgressUI() {
        ThreadManager.getInstance().postUi(() -> mView.showSearchProgressUI());
    }
    public void hideSearchProgressUI() {
        ThreadManager.getInstance().postUi(() -> mView.hideSearchProgressUI());
    }
    public void showTripDialog(String title, String content) {
        ThreadManager.getInstance().postUi(() -> mView.showTripDialog(title, content));
    }
    public void setRouteResultListUI(List<RouteLineInfo> routeLineInfos) {
        ThreadManager.getInstance().postUi(() -> mView.setRouteResultListUI(routeLineInfos));
    }
    public void setViaListUI(List<RouteParam> routeParams) {
        if (routeParams.size() <= NumberUtils.NUM_0) {
            viaPoiListVisibility.set(false);
            viaPoiListAllVisibility.set(false);
            title.set(endName.get());
        } else {
            title.set(AppContext.mContext.getString(R.string.route_my_location));
            viaPoiListVisibility.set(true);
            viaPoiListAllVisibility.set(true);
            String stringParam = ResourceUtils.Companion.getInstance().getString(R.string.route_via_head) + routeParams.size() + ResourceUtils.Companion.getInstance().getString(R.string.route_via_palce);
            for (int t = NumberUtils.NUM_0; t < routeParams.size(); t++) {
                if (t == routeParams.size() - NumberUtils.NUM_1) {
                    stringParam += routeParams.get(t).name;
                    continue;
                }
                stringParam += routeParams.get(t).name + "，";
            }
            paramDes.set(stringParam);
            mView.setViaList(routeParams);
        }
    }
    public void changeParamListMode(int currentPosition, int movePosition) {
        mModel.changeParamListMode(currentPosition, movePosition);
        mModel.requestRouteMode(null, -1, RouteWayID.ROUTE_WAY_SORT_VIA);
    }
    public void deleteViaParamMode(int index) {
        mModel.deleteViaParamMode(index);
    }
    public void showWeatherDeatilsUI(RouteWeatherInfo routeWeatherInfo) {
        weatherName.set(routeWeatherInfo.mWeatherName);
        weatherCityName.set(routeWeatherInfo.mCityName);
        weatherTimeAndDistance.set(TimeUtils.getInstance().getDistanceString(routeWeatherInfo.mDistance) + " " + TimeUtils.getInstance().getTimeStr(routeWeatherInfo.mEta));
        weatherDiscription.set(routeWeatherInfo.mText);
        weatherUpdateTime.set(ResourceUtils.Companion.getInstance().getString(R.string.route_weather_update_head)
                + TimeUtils.getInstance().getTimeStr(System.currentTimeMillis()/1000 - routeWeatherInfo.mTimestamp)
                + ResourceUtils.Companion.getInstance().getString(R.string.route_weather_update_end));
        currentPageHistory.add("4");
        includePageVisibility.set(getCurrentPageUI());
    }
    public void hideWeatherDeatilsUI() {
        removeCurrentPageUI("4");
        includePageVisibility.set(getCurrentPageUI());
    }
    public void hideRouteSearchListUI() {
        removeCurrentPageUI("2");
        includePageVisibility.set(getCurrentPageUI());
    }
    public void showRouteSearchListUI(List<PoiInfoEntity> poiInfoEntities) {
        currentPageHistory.add("2");
        includePageVisibility.set(getCurrentPageUI());
        mView.showRouteSearchListUI(poiInfoEntities);
    }
    public void hideRouteSearchChargeListUI() {
        removeCurrentPageUI("3");
        includePageVisibility.set(getCurrentPageUI());
    }
    public void showRouteSearchChargeListUI(List<PoiInfoEntity> poiInfoEntities, List<RouteParam> gasChargeAlongList  ,int listSearchType) {
        if (getCurrentPageUI() != 3) {
            currentPageHistory.add("3");
            includePageVisibility.set(getCurrentPageUI());
            RouteLineInfo routeLineInfo = mView.getSelectLineInfo();
            routeChargeTotalMileage.set(ResourceUtils.Companion.getInstance().getString(R.string.route_total_mileage)
                    + routeLineInfo.getLength());
            //TODO 缺少正式UI暂不显示
//            routeProgressChargeVisibility.set(!mView.getEnergyChecked());
            EvRangeOnRouteInfo evRangeOnRouteInfo = mModel.getRangeOnRouteInfo(mView.getCurrentRouteIndex());
            if (evRangeOnRouteInfo.isCanArrived()) {
                routeProgressChargeExhaustVisibility.set(false);
                routeProgressChargeExhaust.set(100);
            } else {
                routeProgressChargeExhaustVisibility.set(true);
                int progress = Math.round(((float) (routeLineInfo.getDis() - evRangeOnRouteInfo.getRemainRangeDistance()) / routeLineInfo.getDis()) * 100);
                routeProgressChargeExhaust.set(progress);
                mView.updateRouteChargeExhaustUi(progress / 100.0f);
            }

            mView.highlightAlongTab();
        }
        alongTabSearchVisibility.set(listSearchType == 0);
        mView.showRouteSearchChargeListUI(poiInfoEntities, gasChargeAlongList ,listSearchType);
    }

    public void hideRouteSearchDetailsUI() {
        removeCurrentPageUI("5");
        includePageVisibility.set(getCurrentPageUI());
    }
    public void showRouteSearchDetailsUI(PoiInfoEntity requestPoiInfoEntity, PoiInfoEntity resultPoiInfoEntity, List<RouteParam> gasChargeAlongList, int searchType) {
        mDetailsEntry = requestPoiInfoEntity;
        mDetailsResustEntry = resultPoiInfoEntity;
        routeSearchName.set(requestPoiInfoEntity.getName());
        routeSearchAddress.set(requestPoiInfoEntity.getAddress());
        mModel.getTravelTimeFuture(new GeoPoint(requestPoiInfoEntity.getPoint().lon,requestPoiInfoEntity.getPoint().lat))
                .thenAccept(pair -> {
                    routeSearchTimeAndDistance.set(MessageFormat.format("{0}  {1}", pair.first, pair.second));
                })
                .exceptionally(error -> {
                    Logger.d(TAG, "getTravelTimeFuture error:" + error);
                    return null;
                });
        routeSearchElec.set("20%");
        mSeartype = searchType;
        mGasChargeAlongList = gasChargeAlongList;
        if (mSeartype == 0) {
            if (isBelongSamePoi(gasChargeAlongList, requestPoiInfoEntity)) routeSearchDeailsAddRemoveVia.set(ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.route_service_details_remove_via_charge));
            else routeSearchDeailsAddRemoveVia.set(ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.route_service_details_add_via_charge));
        } else {
            if (mModel.isBelongRouteParam(requestPoiInfoEntity)) routeSearchDeailsAddRemoveVia.set(ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.route_service_details_remove_via));
            else routeSearchDeailsAddRemoveVia.set(ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.route_service_details_add_via));
        }
        if (!ConvertUtils.isEmpty(requestPoiInfoEntity.getServiceAreaInfoList()) &&requestPoiInfoEntity.getServiceAreaInfoList().size() > 0 && !ConvertUtils.isEmpty(requestPoiInfoEntity.getServiceAreaInfoList().get(0))) {
            ServiceAreaInfo info = requestPoiInfoEntity.getServiceAreaInfoList().get(0);
            if (!ConvertUtils.isEmpty(info.getName())) routeSearchName.set(info.getName());
            if (!ConvertUtils.isEmpty(info.getAddress())) routeSearchName.set(info.getAddress());
        }
        ThreadManager.getInstance().postUi(() -> {
            int pointType = SearchPackage.getInstance().getPointTypeCode(requestPoiInfoEntity.getPointTypeCode());
            if (requestPoiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_service))
                    || pointType == AutoMapConstant.PointTypeCode.SERVICE_AREA) {
                routeSearchTypeVisibility.set(3);
                mView.showServiceDetailsUI(resultPoiInfoEntity);
            } else if (requestPoiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge))
                    || pointType == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
                routeSearchTypeVisibility.set(2);
                mView.showChargeDetailsUI(resultPoiInfoEntity);
            }
        });
        currentPageHistory.add("5");
        includePageVisibility.set(getCurrentPageUI());
    }

    private boolean isBelongSamePoi(List<RouteParam> mLoaclSaveEntity, PoiInfoEntity poiInfoEntity) {
        if (mLoaclSaveEntity.isEmpty()) {
            return false;
        }
        for (RouteParam param: mLoaclSaveEntity) {
            if (RoutePackage.getInstance().isTheSamePoi(param, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    public void setDefultPlateNumberAndAvoidLimitSave() {
        currentPlateNumber = mModel.getPlateNumber();
        currentavoidLimit = mModel.getAvoidLimit();
        currentPreferences = mModel.getPreferences();
        currentEnergy = mModel.getEnergy();
    }

    public void isRequestRouteForPlateNumberAndAvoidLimitChange() {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapTypeId.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        if (currentPlateNumber.equals(mModel.getPlateNumber()) && currentavoidLimit.equals(mModel.getAvoidLimit())
                && currentEnergy.equals(mModel.getEnergy()) && currentPreferences.equals(mModel.getPreferences())) return;
        restrictionVisibility.set(false);
        if (!currentEnergy.equals(mModel.getEnergy())) {
            setDefultPlateNumberAndAvoidLimitSave();
            mView.setEnergyChecked();
            return;
        }
        setDefultPlateNumberAndAvoidLimitSave();
        mModel.requestRouteMode(null, -1, RouteWayID.ROUTE_WAY_DEFAULT);
    }
    public void updateRestrictionTextUI(int routeRestrictionType) {
        restrictionVisibility.set(true);
        restrictionRightBackVisibility.set(false);
        mRestrictionStatus = routeRestrictionType;
        restrictionBackground.set(ResourceUtils.Companion.getInstance().getDrawable(R.color.text_route_restriction_error));
        restrictionTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().getColor(R.color.text_route_restriction_text_error));
        switch (routeRestrictionType) {
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENOPLATE:
                restriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstypenoplate));
                restrictionRightBackVisibility.set(true);
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENOTOPEN:
                restriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstypenotopen));
                restrictionRightBackVisibility.set(true);
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEAVOIDSUCCESS:
                restriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstypeavoidsuccess));
                restrictionBackground.set(ResourceUtils.Companion.getInstance().getDrawable(R.color.text_route_restriction_success));
                restrictionTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().getColor(R.color.text_route_restriction_text_success));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEREGIONSTART:
                restriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstyperegionstart));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEREGIONEND:
                restriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstyperegionend));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEREGIONVIA:
                restriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstyperegionvia));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEREGIONCROSS:
                restriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstyperegioncross));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEINVALID:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEAVOIDFUTURESUCCESS:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEEXPIREDIMMEDIATELY:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEWAITLIMITOFF:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEWAITLIMITOFFSHORT:
                Logger.i(TAG, "hide restriction");
                restrictionVisibility.set(false);
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENETWORK:
                restriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstypenetwork));
                break;
        }
    }
    public void updateSelectRouteUI(int routeIndex) {
        ThreadManager.getInstance().postUi(() -> mView.updateSelectRouteUI(routeIndex));
    }

    public void showHideTab(boolean isLongRoute) {
        tabVisibility.set(isLongRoute ? 2 : 1);
    }

    public void showHideElicCheckBox(boolean b) {
        elecCheckBoxVisibility.set(b);
    }

    public void updateChareList(List<RouteParam> gasChargeAlongList, int listSearchType) {
        mView.updateChareList(gasChargeAlongList, listSearchType);
    }

    @Override
    public void onRoutePreferenceChange(String text, boolean isFirstChange) {
        if (!isFirstChange) {
            requestRoute(null, -1, RouteWayID.ROUTE_WAY_DEFAULT);
        }
        preferText.set(text);
        routePreferenceVisibility.set(false);
        routePreferenceDrawableVisibility.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
    }

    @Override
    public void onResultListUpdate() {
        cancelTimer();
        initTimer();
    }

    @Override
    public void onRouteSelect(boolean isTheSameIndex, int index) {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapTypeId.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        mView.clearSceneTabUI();
        isChargingSelect = false;
        mView.clearSceneTabUI(false);
        if (isTheSameIndex) currentPageHistory.add("1");
        includePageVisibility.set(getCurrentPageUI());
        mView.setDetailsResult(mModel.getDetailsResult(index));
        mModel.selectRoute(index);
    }

    @Override
    public void onListTouch() {
        cancelTimer();
    }

    @Override
    public void onRouteDetailsChecked(boolean checkedLeastOne) {
        avoidBackground.set(checkedLeastOne ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_route_details_avoid_road) : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_route_details_defult_label));
        avoidClickable.set(checkedLeastOne);
    }

    @Override
    public void onTabListClick(int tabIndex, boolean isChecked) {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapTypeId.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        switch (tabIndex) {
            case 0:
                if (isChecked) {
                    mModel.getSearchListChargeAndShow(searchKeyWord, 0);
                } else {
                    hideRouteSearchDetailsUI();
                    hideRouteSearchListUI();
                    hideRouteSearchChargeListUI();
                    mModel.clearSearchLabel();
                }
                break;
            case 1:
                if (isChecked) mModel.getWeatherList();
                else mModel.hideWeatherList();
                break;
            case 2:
                if (isChecked) {
                    mModel.getSearchListAndShow(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_service), 0);
                }
                else {
                    hideRouteSearchDetailsUI();
                    hideRouteSearchListUI();
                    hideRouteSearchChargeListUI();
                    mModel.clearSearchLabel();
                }
                break;
        }
    }

    @Override
    public void onTabListGasChargeClick(int tabIndex) {
        mModel.getSearchListChargeAndShow(searchKeyWord, tabIndex);
    }

    private boolean isChargingSelect = false;
    public Action tabCharging = () -> {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapTypeId.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        if (Boolean.FALSE.equals(isChargingSelect)) {
            mModel.getSearchListChargeAndShow(searchKeyWord, 0);
        } else {
            hideRouteSearchDetailsUI();
            hideRouteSearchListUI();
            mModel.clearSearchLabel();
        }
        isChargingSelect = !isChargingSelect;
        mView.clearSceneTabUI(isChargingSelect);
    };

    @Override
    public void enterToDetails(PoiInfoEntity poiInfoEntity) {
        mModel.getSearchDetailsMode(poiInfoEntity);
    }

    @Override
    public boolean onTouch(View view, MotionEvent motionEvent) {
        cancelTimer();
        return false;
    }

    @Override
    public void enterToChargeDetails(PoiInfoEntity poiInfoEntity) {
        mModel.getSearchDetailsMode(poiInfoEntity);
    }
    @Override
    public void onGasChargeRemoveClick(PoiInfoEntity poiInfoEntity) {
        mModel.gasChargeRemoveMode(poiInfoEntity);
    }
    @Override
    public void onGasChargeAddClick(PoiInfoEntity poiInfoEntity) {
        mModel.gasChargeAddMode(poiInfoEntity);
    }
}
