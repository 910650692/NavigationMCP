package com.fy.navi.hmi.route;


import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.View;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.navi.NaviGuidanceFragment;
import com.fy.navi.hmi.search.alongway.MainAlongWaySearchFragment;
import com.fy.navi.hmi.setting.SettingFragment;
import com.fy.navi.hmi.setting.guide.platenumber.SettingPlateNumberFragment;
import com.fy.navi.scene.api.route.ISceneRouteDetailsSelectCallBack;
import com.fy.navi.scene.api.route.ISceneRouteGasStationChargeSelectCallBack;
import com.fy.navi.scene.api.route.ISceneRouteGasStationWeatherServiceSelectCallBack;
import com.fy.navi.scene.api.route.ISceneRoutePreferenceCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSearchChargeRefreshItemCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSearchRefreshItemCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSelectCallBack;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.search.RouteRequestLoadingDialog;
import com.fy.navi.scene.ui.search.SearchConfirmDialog;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.route.RouteRestirctionID;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.route.RouteWeatherID;
import com.fy.navi.service.define.route.RouteWeatherInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ServiceAreaInfo;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
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

public class BaseRouteViewModel extends BaseViewModel<RouteFragment, RouteModel> implements ISceneRoutePreferenceCallBack
        , ISceneRouteSelectCallBack
        , ISceneRouteDetailsSelectCallBack
        , ISceneRouteSearchRefreshItemCallBack
        , ISceneRouteGasStationWeatherServiceSelectCallBack
        , ISceneRouteGasStationChargeSelectCallBack
        , ISceneRouteSearchChargeRefreshItemCallBack
        , View.OnTouchListener
        , RouteRequestLoadingDialog.OnCloseClickListener {

    private static final String TAG = BaseRouteViewModel.class.getSimpleName();

    private final static String ROUTE_ERROR = "异常";
    private static final int HIDE_DELAY_TIME = 20 * 1000;
    private static final int REFRESH_TIME = 2 * 1000;

    private ObservableField<Integer> mTabVisibility;

    public ObservableField<Integer> getTabVisibility() {
        return mTabVisibility;
    }

    private ObservableField<Boolean> mBatterCheckBoxVisibility;

    public ObservableField<Boolean> getBatterCheckBoxVisibility() {
        return mBatterCheckBoxVisibility;
    }

    private ObservableField<Boolean> mAlongTabSearchVisibility;

    public ObservableField<Boolean> getAlongTabSearchVisibility() {
        return mAlongTabSearchVisibility;
    }

    /**
     * 主页面-详情页
     **/
    private ObservableField<Integer> mIncludePageVisibility;

    public ObservableField<Integer> getIncludePageVisibility() {
        return mIncludePageVisibility;
    }

    /**
     * 主页面-子poi页
     **/
    private PoiInfoEntity mSecondaryPoiInfo;
    public void setSecondaryPoiInfo(PoiInfoEntity poiInfoEntity) {
        mSecondaryPoiInfo = poiInfoEntity;
    }
    private ObservableField<Integer> mSecondaryPoiVisibility;

    public ObservableField<Integer> getSecondaryPoiVisibility() {
        return mSecondaryPoiVisibility;
    }

    /**
     * 主页面-偏好页
     **/
    private ObservableField<Boolean> mRoutePreferenceVisibility;

    public ObservableField<Boolean> getRoutePreferenceVisibility() {
        return mRoutePreferenceVisibility;
    }

    /**
     * 主页面-偏好ICON
     **/
    private ObservableField<Drawable> mRoutePreferenceDrawableVisibility;

    public ObservableField<Drawable> getRoutePreferenceDrawableVisibility() {
        return mRoutePreferenceDrawableVisibility;
    }

    /**
     * 主页面-标题-终点名称~我的位置
     **/
    private ObservableField<String> mTitle;

    public ObservableField<String> getTitle() {
        return mTitle;
    }

    /**
     * 主页面-限行文本
     **/
    private ObservableField<String> mRestriction;

    public ObservableField<String> getRestriction() {
        return mRestriction;
    }

    /**
     * 主页面-限行文本颜色
     **/
    private ObservableField<Integer> mRestrictionTextColor;

    public ObservableField<Integer> getRestrictionTextColor() {
        return mRestrictionTextColor;
    }

    /**
     * 主页面-限行背景
     **/
    private ObservableField<Drawable> mRestrictionBackground;

    public ObservableField<Drawable> getRestrictionBackground() {
        return mRestrictionBackground;
    }

    /**
     * 主页面-限行可见
     **/
    private ObservableField<Boolean> mRestrictionVisibility;

    public ObservableField<Boolean> getRestrictionVisibility() {
        return mRestrictionVisibility;
    }

    /**
     * 主页面-限行左侧车标可见
     **/
    private ObservableField<Boolean> mRestrictionCarVisibility;

    public ObservableField<Boolean> getRestrictionCarVisibility() {
        return mRestrictionCarVisibility;
    }

    /**
     * 主页面-限行右侧back可见
     **/
    private ObservableField<Boolean> mRestrictionRightBackVisibility;

    public ObservableField<Boolean> getRestrictionRightBackVisibility() {
        return mRestrictionRightBackVisibility;
    }

    /**
     * 主页面-途经点—终点名称
     **/
    private ObservableField<String> mEndName;

    public ObservableField<String> getEndName() {
        return mEndName;
    }

    /**
     * 主页面-偏好名称
     **/
    private ObservableField<String> mPreferText;

    public ObservableField<String> getPreferText() {
        return mPreferText;
    }

    /**
     * 主页面-开始导航文本
     **/
    private ObservableField<String> mCountDownHint;

    public ObservableField<String> getCountDownHint() {
        return mCountDownHint;
    }

    /**
     * 主页面-途径点-缩小描述
     **/
    private ObservableField<String> mParamDes;

    public ObservableField<String> getParamDes() {
        return mParamDes;
    }

    /**
     * 主页面-有无途经点
     **/
    private ObservableField<Boolean> mViaPoiListAllVisibility;

    public ObservableField<Boolean> getViaPoiListAllVisibility() {
        return mViaPoiListAllVisibility;
    }

    /**
     * 主页面-途经点是否展开
     **/
    private ObservableField<Boolean> mRouteDetailsVisibility;

    public ObservableField<Boolean> getRouteDetailsVisibility() {
        return mRouteDetailsVisibility;
    }

    /**
     * 详情页面-避开按钮背景
     **/
    private ObservableField<Boolean> mAvoidBackground;

    public ObservableField<Boolean> getAvoidBackground() {
        return mAvoidBackground;
    }

    /**
     * 详情页面-避开按钮文本颜色
     **/
    private ObservableField<Integer> mAvoidTestColor;

    public ObservableField<Integer> getAvoidTestColor() {
        return mAvoidTestColor;
    }

    /**
     * 详情页面-避开按钮是否可点击
     **/
    private ObservableField<Boolean> mAvoidClickable;

    public ObservableField<Boolean> getAvoidClickable() {
        return mAvoidClickable;
    }

    /**
     * 详情页面-避开按钮背景
     **/
    private ObservableField<Boolean> mViaPoiListVisibility;

    public ObservableField<Boolean> getViaPoiListVisibility() {
        return mViaPoiListVisibility;
    }

    /**
     * 搜索列表页面-标题
     **/
    private ObservableField<String> mSearchListTitle;

    public ObservableField<String> getSearchListTitle() {
        return mSearchListTitle;
    }

    /**
     * 天气详情页面
     **/
    private ObservableField<Drawable> mWeatherDrawable;

    public ObservableField<Drawable> getWeatherDrawable() {
        return mWeatherDrawable;
    }

    private ObservableField<String> mWeatherName;

    public ObservableField<String> getWeatherName() {
        return mWeatherName;
    }

    private ObservableField<String> mWeatherCityName;

    public ObservableField<String> getWeatherCityName() {
        return mWeatherCityName;
    }

    private ObservableField<String> mWeatherTimeAndDistance;

    public ObservableField<String> getWeatherTimeAndDistance() {
        return mWeatherTimeAndDistance;
    }

    private ObservableField<String> mWeatherDiscription;

    public ObservableField<String> getWeatherDiscription() {
        return mWeatherDiscription;
    }

    private ObservableField<String> mWeatherUpdateTime;

    public ObservableField<String> getWeatherUpdateTime() {
        return mWeatherUpdateTime;
    }

    /**
     * POI详情页面
     **/
    private ObservableField<Boolean> mRouteSearchStatusVisibility;

    public ObservableField<Boolean> getRouteSearchStatusVisibility() {
        return mRouteSearchStatusVisibility;
    }

    private ObservableField<String> mRouteSearchStatus;

    public ObservableField<String> getRouteSearchStatus() {
        return mRouteSearchStatus;
    }

    private ObservableField<String> mRouteSearchName;

    public ObservableField<String> getRouteSearchName() {
        return mRouteSearchName;
    }

    private ObservableField<String> mRouteSearchAddress;

    public ObservableField<String> getRouteSearchAddress() {
        return mRouteSearchAddress;
    }

    private ObservableField<String> mRouteSearchTimeAndDistance;

    public ObservableField<String> getRouteSearchTimeAndDistance() {
        return mRouteSearchTimeAndDistance;
    }

    private ObservableField<String> mRouteSearchElec;

    public ObservableField<String> getRouteSearchElec() {
        return mRouteSearchElec;
    }

    private ObservableField<Integer> mRouteSearchTypeVisibility;

    public ObservableField<Integer> getRouteSearchTypeVisibility() {
        return mRouteSearchTypeVisibility;
    }

    private ObservableField<String> mRouteSearchDeailsAddRemoveVia;

    public ObservableField<String> getRouteSearchDeailsAddRemoveVia() {
        return mRouteSearchDeailsAddRemoveVia;
    }

    /**
     * POI充电站页面
     **/
    private ObservableField<String> mRouteChargeTotalMileage;

    public ObservableField<String> getRouteChargeTotalMileage() {
        return mRouteChargeTotalMileage;
    }

    private ObservableField<Boolean> mRouteProgressChargeVisibility;

    public ObservableField<Boolean> getRouteProgressChargeVisibility() {
        return mRouteProgressChargeVisibility;
    }

    private ObservableField<Boolean> mRouteProgressChargeExhaustVisibility;

    public ObservableField<Boolean> getRouteProgressChargeExhaustVisibility() {
        return mRouteProgressChargeExhaustVisibility;
    }

    private ObservableField<Integer> mRouteProgressChargeExhaust;

    public ObservableField<Integer> getRouteProgressChargeExhaust() {
        return mRouteProgressChargeExhaust;
    }

    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_9;

    private int mRestrictionStatus = 0;
    private String mCurrentPlateNumber;
    private String mCurrentavoidLimit;
    private String mCurrentPreferences;
    private String mCurrentEnergy;
    private String mSearchKeyWord = ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge);
    private String mGasSearchKeyWord = ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_gas);
    private long mRouteTotalDistance;
    //计算能耗点距离
    private long mExhaustDistance;
    //算法返回能耗点距离
    private long mRouteExhaustDistance;
    private List<Integer> mChargePoiDistanceList = new ArrayList<>();
    private List<String> mCurrentPageHistory = new ArrayList<>();
    private PoiInfoEntity mDetailsEntry;
    private PoiInfoEntity mDetailsResustEntry;
    private int mSeartype;
    private int mSearchListType = 0;
    private List<RouteParam> mGasChargeAlongList;
    private Runnable mHideSecondaryPoi = new Runnable() {
        @Override
        public void run() {
            if (mSecondaryPoiVisibility != null) {
                mSecondaryPoiVisibility.set(0);
            }
        }
    };
    private boolean mSecondaryPoi = false;
    private boolean mRefreshable = true;
    private Runnable mRefreshTimer = new Runnable() {
        @Override
        public void run() {
            mRefreshable = true;
        }
    };

    public BaseRouteViewModel(final Application application) {
        super(application);
        mCurrentPageHistory.add("0");
        mIncludePageVisibility = new ObservableField<>(getCurrentPageUI());
        mSecondaryPoiVisibility = new ObservableField<>(0);
        mTabVisibility = new ObservableField<>(0);
        mBatterCheckBoxVisibility = new ObservableField<>(false);
        mAlongTabSearchVisibility = new ObservableField<>(true);
        mRoutePreferenceVisibility = new ObservableField<>(false);
        mRoutePreferenceDrawableVisibility = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
        mCountDownHint = new ObservableField<>(ResourceUtils.Companion.getInstance().getString(R.string.route_start_navi)
                + "(" + NumberUtils.NUM_9 + "s)");
        mRestriction = new ObservableField<>("");
        mRestrictionBackground = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.color.text_route_restriction_error));
        mRestrictionTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().getColor(R.color.text_route_restriction_text_error));
        mRoutePreferenceDrawableVisibility = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));

        mRestrictionVisibility = new ObservableField<>(false);
        mRestrictionCarVisibility = new ObservableField<>(false);
        mRestrictionRightBackVisibility = new ObservableField<>(false);
        mParamDes = new ObservableField<>("");
        mAvoidBackground = new ObservableField<>(false);
        mAvoidTestColor = new ObservableField<>(ResourceUtils.Companion.getInstance().getColor(R.color.text_route_defult));
        mAvoidClickable = new ObservableField<>(false);
        mTitle = new ObservableField<>();
        mEndName = new ObservableField<>();
        mPreferText = new ObservableField<>(ResourceUtils.Companion.getInstance().getString(R.string.route_change_preference));
        mRouteDetailsVisibility = new ObservableField<>(true);
        mViaPoiListVisibility = new ObservableField<>(false);
        mViaPoiListAllVisibility = new ObservableField<>(false);
        mSearchListTitle = new ObservableField<>(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge));

        mWeatherDrawable = new ObservableField<>(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_weather_clear));
        mWeatherName = new ObservableField<>("");
        mWeatherCityName = new ObservableField<>("");
        mWeatherTimeAndDistance = new ObservableField<>("");
        mWeatherDiscription = new ObservableField<>("");
        mWeatherUpdateTime = new ObservableField<>("");

        mRouteSearchStatusVisibility = new ObservableField<>(false);
        mRouteSearchStatus = new ObservableField<>("");
        mRouteSearchName = new ObservableField<>("");
        mRouteSearchAddress = new ObservableField<>("");
        mRouteSearchTimeAndDistance = new ObservableField<>("");
        mRouteSearchElec = new ObservableField<>("");
        mRouteSearchTypeVisibility = new ObservableField<>(0);
        mRouteSearchDeailsAddRemoveVia = new ObservableField<>("");
        mRouteChargeTotalMileage = new ObservableField<>("");
        mRouteProgressChargeVisibility = new ObservableField<>(false);
        mRouteProgressChargeExhaustVisibility = new ObservableField<>(true);
        mRouteProgressChargeExhaust = new ObservableField<>(0);
    }

    /***
     * 获取当前展示页面
     * @return 当前页面id
     */
    private Integer getCurrentPageUI() {
        Logger.i(TAG, "getCurrentPageUI: ==>");
        if (mCurrentPageHistory.isEmpty()) {
            return 0;
        }
        final int index = Integer.parseInt(mCurrentPageHistory.get(mCurrentPageHistory.size() - 1));
        if (index == 0) {
            //回到算路结果页需要全览路线
            ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        }
        Logger.i(TAG, "getCurrentPageUI: " + index);
        return index;
    }

    /***
     * 移除当前展示页面
     * @param pageId 页面id
     */
    private void removeCurrentPageUI(final String pageId) {
        if (mCurrentPageHistory.isEmpty()) {
            return;
        }
        if (mCurrentPageHistory.contains(pageId)) {
            mCurrentPageHistory.remove(pageId);
        }
    }

    @Override
    protected RouteModel initModel() {
        return new RouteModel();
    }

    private Action mCloseRouteClick = () -> {
        Logger.i(TAG, "closeRoute: ==>");
        if (Boolean.TRUE.equals(mRoutePreferenceVisibility.get())) {
            mRoutePreferenceVisibility.set(false);
            mRoutePreferenceDrawableVisibility.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
            return;
        }
        cancelTimer();
        mModel.clearRouteLine();
        mModel.clearRestrictionView();
        mView.hideTrip();
        closeFragment(true);
    };

    public Action getCloseRouteClick() {
        return mCloseRouteClick;
    }

    private Action mRestrictionClick = () -> {
        cancelTimer();
        switch (mRestrictionStatus) {
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENOPLATE:
                addFragment(new SettingFragment(), null);
                addFragment(new SettingPlateNumberFragment(), null);
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENOTOPEN:
                addFragment(new SettingFragment(), null);
                break;
            default:
                mModel.showRestrictionFragment();
                break;
        }
    };

    public Action getRestrictionClick() {
        return mRestrictionClick;
    }

    private Action mPreferenceClick = () -> {
        cancelTimer();
        mRoutePreferenceVisibility.set(Boolean.FALSE.equals(mRoutePreferenceVisibility.get()));
        mRoutePreferenceDrawableVisibility.set(Boolean.TRUE.equals(mRoutePreferenceVisibility.get())
                ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_up)
                : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
    };

    public Action getPreferenceClick() {
        return mPreferenceClick;
    }

    private Action mAlongWaySearchClick = () -> {
        cancelTimer();
        addFragment(new MainAlongWaySearchFragment(), null);
    };

    public Action getAlongWaySearchClick() {
        return mAlongWaySearchClick;
    }

    private Action mOpenCloseViaClick = () -> {
        cancelTimer();
        mRoutePreferenceVisibility.set(false);
        mRoutePreferenceDrawableVisibility.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
        mViaPoiListAllVisibility.set(Boolean.FALSE.equals(mViaPoiListAllVisibility.get()));
    };

    public Action getOpenCloseViaClick() {
        return mOpenCloseViaClick;
    }

    private Action mRouteEnergyClick = () -> {
        cancelTimer();
        mView.setEnergyChecked();
    };

    public Action getRouteEnergyClick() {
        return mRouteEnergyClick;
    }


    private Action mStartNaviClick = () -> {
        cancelTimer();
        mModel.clearSearchLabel();
        mModel.clearWeatherView();
        mView.hideTrip();
        startNavi(false);
    };

    public Action getStartNaviClick() {
        return mStartNaviClick;
    }

    private Action mRefreshRouteClick = () -> {
        if (!mRefreshable) {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.route_refresh_fast));
            return;
        }
        cancelTimer();
        final RouteRequestParam param = new RouteRequestParam();
        param.setMRouteWay(RouteWayID.ROUTE_WAY_REFRESH);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH);
        requestRoute(param);
        mRefreshable = false;
        ThreadManager.getInstance().postDelay(mRefreshTimer, REFRESH_TIME);
        showSecondaryPoi();
    };

    public Action getRefreshRouteClick() {
        return mRefreshRouteClick;
    }

    //路线详情页面
    private Action mCloseDetailsRouteClick = () -> {
        if (Boolean.FALSE.equals(mRouteDetailsVisibility.get())) {
            mRouteDetailsVisibility.set(true);
            mView.setAvoidStatusUI(false);
            return;
        }
        mCurrentPageHistory.remove(mCurrentPageHistory.size() - 1);
        mIncludePageVisibility.set(getCurrentPageUI());
    };

    public Action getCloseDetailsRouteClick() {
        return mCloseDetailsRouteClick;
    }

    private Action mStartSimNaviClick = () -> {
        mModel.clearSearchLabel();
        mModel.clearWeatherView();
        mView.hideTrip();
        startNavi(true);
    };

    public Action getStartSimNaviClick() {
        return mStartSimNaviClick;
    }

    private Action mAvoidRoadClick = () -> {
        mRouteDetailsVisibility.set(false);
        mView.setAvoidStatusUI(true);
    };

    public Action getAvoidRoadClick() {
        return mAvoidRoadClick;
    }

    private Action mAvoidClick = () -> mView.startAvoidRoad();

    public Action getAvoidClick() {
        return mAvoidClick;
    }

    //天气详情
    private Action mCloseWeatherDetailsClick = this::hideWeatherDeatilsUI;

    public Action getCloseWeatherDetailsClick() {
        return mCloseWeatherDetailsClick;
    }

    private Action mWeatherAviodClick = this::hideWeatherDeatilsUI;

    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_ROUTE_WEATHER_AVOID)
    public Action getWeatherAviodClick() {
        return mWeatherAviodClick;
    }

    private Action mServiceRefreshClick = () -> ToastUtils.Companion.getInstance().showCustomToastView(
            ResourceUtils.Companion.getInstance().getString(R.string.route_no_function));

    public Action getServiceRefreshClick() {
        return mServiceRefreshClick;
    }

    private Action mCloseServicelistClick = () -> {
        mView.clearSceneTabUI();
        mIsChargingSelect = false;
        mIsGasSelect = false;
        mView.clearSceneTabUI(false);
        mView.clearSceneGasTabUI(false);
        hideRouteSearchListUI();
        mModel.clearSearchLabel();
        mModel.clearRestArea();
    };

    public Action getCloseServicelistClick() {
        return mCloseServicelistClick;
    }

    private Action mCloseChargelistClick = () -> {
        mView.clearSceneTabUI();
        mIsChargingSelect = false;
        mIsGasSelect = false;
        mView.clearSceneTabUI(false);
        mView.clearSceneGasTabUI(false);
        hideRouteSearchChargeListUI();
        mModel.clearSearchLabel();
    };

    public Action getCloseChargelistClick() {
        return mCloseChargelistClick;
    }

    private Action mChargeRefreshClick = () -> ToastUtils.Companion.getInstance().showCustomToastView(
            ResourceUtils.Companion.getInstance().getString(R.string.route_no_function));

    public Action getChargeRefreshClick() {
        return mChargeRefreshClick;
    }

    //POI
    private Action mCloseRouteSearchDetailClick = this::hideRouteSearchDetailsUI;

    public Action getCloseRouteSearchDetailClick() {
        return mCloseRouteSearchDetailClick;
    }

    private Action mRouteSearchDetailsAddRemoveClick = () -> {
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

    public Action getRouteSearchDetailsAddRemoveClick() {
        return mRouteSearchDetailsAddRemoveClick;
    }

    private Action mRouteSearchPhoneClick = () -> {
        if (!ConvertUtils.isEmpty(mDetailsResustEntry) && !ConvertUtils.isEmpty(mDetailsResustEntry.getPhone())) {
            final String phone = mDetailsResustEntry.getPhone();
            final List<String> phoneString = new ArrayList<>();
            if (phone.contains(";")) {
                final String[] split = phone.split(";");
                phoneString.addAll(Arrays.asList(split));
            } else {
                phoneString.add(phone);
            }
            if (!ConvertUtils.isEmpty(phoneString) && !ConvertUtils.isEmpty(phoneString.get(0))) {
                Logger.d(TAG, "call phone: " + phoneString.get(0));
                new SearchConfirmDialog.Build(mView.getContext())
                        .setDialogObserver(new IBaseDialogClickListener() {
                            @Override
                            public void onCommitClick() {
                                //拨打电话
                                final Intent intent = new Intent();
                                intent.setAction(Intent.ACTION_CALL);
                                intent.setData(Uri.parse("tel:" + phoneString.get(0)));
                                final Context context = mView.getContext();
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

    public Action getRouteSearchPhoneClick() {
        return mRouteSearchPhoneClick;
    }

    private Action mRouteSearchAroundClick = () -> {
    };

    public Action getRouteSearchAroundClick() {
        return mRouteSearchAroundClick;
    }

    private Action mRouteSearchFavoriteClick = () -> {
    };

    public Action getRouteSearchFavoriteClick() {
        return mRouteSearchFavoriteClick;
    }

    private Action mChargeSureClick = () -> {
        mModel.startAllRequest();
    };

    public Action getChargeSureClick() {
        return mChargeSureClick;
    }

    private Action mChargeCancelClick = () -> {
        mView.clearSceneTabUI();
        mIsChargingSelect = false;
        mIsGasSelect = false;
        mView.clearSceneTabUI(false);
        mView.clearSceneGasTabUI(false);
        hideRouteSearchChargeListUI();
        mModel.clearSearchLabel();
    };

    public Action getChargeCancelClick() {
        return mChargeCancelClick;
    }

    /***
     * 页面倒计时
     */
    public void initTimer() {
        if (NaviStatus.NaviStatusType.NAVING.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            return;
        }
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(() -> {
                    cancelTimer();
                    mModel.clearSearchLabel();
                    mModel.clearWeatherView();
                    mView.hideTrip();
                    startNavi(false);
                });
            } else {
                final String hint = ResourceUtils.Companion.getInstance().getString(R.string.route_start_navi) + "(" + mTimes + "s)";
                mCountDownHint.set(hint);
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    /***
     * 取消页面倒计时
     */
    public void cancelTimer() {
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mCountDownHint.set(ResourceUtils.Companion.getInstance().getString(R.string.route_start_navi));
            mTimes = NumberUtils.NUM_9;
            mScheduledFuture = null;
        }
    }

    /***
     * 开始导航
     * @param isSimNavi 是否模拟导航
     */
    public void startNavi(final boolean isSimNavi) {
        final Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, isSimNavi
                ? AutoMapConstant.NaviType.NAVI_SIMULATE : AutoMapConstant.NaviType.NAVI_GPS);
        Logger.i(TAG, "mStartNaviClick addNaviFragment");
        mModel.clearEndParkPoint();
        addFragment(new NaviGuidanceFragment(), bundle);
        mModel.setPoint();
    }

    /***
     * 请求算路
     * @param param 请求参数
     */
    public void requestRoute(final RouteRequestParam param) {
        mModel.requestRouteMode(param);
    }

    /***
     * send2car 请求算路
     * @param routeMsgPushInfo 请求参数
     */
    public void requestRouteRestoration(final RouteMsgPushInfo routeMsgPushInfo) {
        mModel.requestRouteRestoration(routeMsgPushInfo);
    }

    /***
     * 语音 请求算路
     * @param param 请求参数
     */
    public void requestRouteFromSpeech(final RouteSpeechRequestParam param) {
        mModel.requestRouteFromSpeech(param);
    }

    /***
     * 请求成功-初始页面
     */
    public void showNomalRouteUI() {
        mView.clearSceneTabUI();
        mIsChargingSelect = false;
        mIsGasSelect = false;
        mView.clearSceneTabUI(false);
        mView.clearSceneGasTabUI(false);
        mCurrentPageHistory.clear();
        mCurrentPageHistory.add("0");
        mIncludePageVisibility.set(getCurrentPageUI());
        mRoutePreferenceVisibility.set(false);
        mRoutePreferenceDrawableVisibility.set(Boolean.TRUE.equals(mRoutePreferenceVisibility.get())
                ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_up)
                : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
        mRouteDetailsVisibility.set(true);
        mAvoidBackground.set(false);
        mAvoidTestColor.set(ResourceUtils.Companion.getInstance().getColor(R.color.text_route_defult));
        mAvoidClickable.set(false);
        mView.setAvoidStatusUI(false);
        updateRestrictionTextUI(-1);
    }

    /***
     * 展示算路弹框
     */
    public void showProgressUI() {
        ThreadManager.getInstance().postUi(() -> {
            mView.showProgressUI();
            //重算路关闭Route/Guidance上面所有页面
            closeAllFragmentUpRoute();
        });
    }


    /***
     * 只展示算路中弹框
     */
    public void showProgressUIOnly() {
        ThreadManager.getInstance().postUi(() -> {
            mView.showProgressUI();
        });
    }

    /***
     * 展示离线算路中弹框
     */
    public void showOfflineProgressUI() {
        ThreadManager.getInstance().postUi(() -> {
            mView.showOfflineProgressUI();
        });
    }

    /***
     * 隐藏算路弹框
     * @param success 算路成功
     */
    public void hideProgressUI(final boolean success) {
        ThreadManager.getInstance().postUi(() -> {
            mView.hideProgressUI();
            if (success) {
                cancelTimer();
                initTimer();
            }
        });
    }

    /***
     * 展示搜索弹框
     */
    public void showSearchProgressUI() {
        ThreadManager.getInstance().postUi(() -> mView.showSearchProgressUI());
    }

    /***
     * 隐藏搜索弹框
     */
    public void hideSearchProgressUI() {
        ThreadManager.getInstance().postUi(() -> mView.hideSearchProgressUI());
    }

    /***
     * 展示Trip弹框
     * @param title 标题
     * @param content 内容
     */
    public void showTripDialog(final String title, final String content) {
        ThreadManager.getInstance().postUi(() -> mView.showTripDialog(title, content));
    }

    /***
     * 展示算路结果列表
     * @param routeLineInfos 列表数据
     */
    public void setRouteResultListUI(final List<RouteLineInfo> routeLineInfos) {
        ThreadManager.getInstance().postUi(() -> mView.setRouteResultListUI(routeLineInfos));
    }

    /***
     * 展示途经点列表
     * @param routeParams 列表数据
     */
    public void setViaListUI(final List<RouteParam> routeParams) {
        if (routeParams.size() <= NumberUtils.NUM_0) {
            mViaPoiListVisibility.set(false);
            mViaPoiListAllVisibility.set(false);
            mTitle.set(mEndName.get());
        } else {
            mTitle.set(AppContext.getInstance().getMContext().getString(R.string.route_my_location));
            mViaPoiListVisibility.set(true);
            String stringParam = ResourceUtils.Companion.getInstance().getString(R.string.route_via_head)
                    + routeParams.size() + ResourceUtils.Companion.getInstance().getString(R.string.route_via_palce);
            for (int t = NumberUtils.NUM_0; t < routeParams.size(); t++) {
                if (t == routeParams.size() - NumberUtils.NUM_1) {
                    stringParam += routeParams.get(t).getName();
                    continue;
                }
                stringParam += routeParams.get(t).getName() + "，";
            }
            mParamDes.set(stringParam);
            mView.setViaList(routeParams);
        }
    }

    /***
     * 排序途经点列表
     * @param currentPosition 当前索引
     * @param movePosition 移动的索引
     */
    public void changeParamListMode(final int currentPosition, final int movePosition) {
        mModel.changeParamListMode(currentPosition, movePosition);
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
        param.setMRouteWay(RouteWayID.ROUTE_WAY_SORT_VIA);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
        requestRoute(param);
    }

    /***
     * 删除途经点
     * @param index 索引
     */
    public void deleteViaParamMode(final int index) {
        mModel.deleteViaParamMode(index);
    }

    /***
     * 展示天气详情
     * @param routeWeatherInfo 详情数据
     */
    public void showWeatherDeatilsUI(final RouteWeatherInfo routeWeatherInfo) {
        mWeatherDrawable.set(getWeatherIcon(routeWeatherInfo.getMRouteWeatherID()));
        mWeatherName.set(routeWeatherInfo.getMWeatherName());
        mWeatherCityName.set(routeWeatherInfo.getMCityName());
        mWeatherTimeAndDistance.set(TimeUtils.getInstance().getDistanceString(routeWeatherInfo.getMDistance())
                + " " + TimeUtils.getInstance().getTimeStr(routeWeatherInfo.getMEta()));
        mWeatherDiscription.set(routeWeatherInfo.getMText());
        mWeatherUpdateTime.set(ResourceUtils.Companion.getInstance().getString(R.string.route_weather_update_head)
                + TimeUtils.getInstance().getTimeStr(System.currentTimeMillis() / 1000 - routeWeatherInfo.getMTimestamp())
                + ResourceUtils.Companion.getInstance().getString(R.string.route_weather_update_end));
        if (getCurrentPageUI() != 4) {
            mCurrentPageHistory.add("4");
            mIncludePageVisibility.set(getCurrentPageUI());
        }
    }

    /***
     * 获取天气图片
     * @param id weatherId
     * @return 图片
     */
    private Drawable getWeatherIcon(final RouteWeatherID id) {
        Drawable drawable = null;
        switch (id) {
            case ROUTE_WEATHER_CLOUDY ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_cloud);
            case ROUTE_WEATHER_THUNDER ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_thunder);
            case ROUTE_WEATHER_MORE_CLOUDY ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_more_cloud);
            case ROUTE_WEATHER_RAIN ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_rain);
            case ROUTE_WEATHER_SNOW ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_snow);
            case ROUTE_WEATHER_BIG_RAIN ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_big_rain);
            case ROUTE_WEATHER_WIND ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_wind);
            case ROUTE_WEATHER_FOG ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_fog);
            case ROUTE_WEATHER_HAIL ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_hail);
            default ->
                    drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_weather_sunny);
        }
        return drawable;
    }

    /***
     * 显示子POI界面
     */
    public void showSecondaryPoi() {
        if (!mSecondaryPoi) {
            return;
        }
        mSecondaryPoiVisibility.set(1);
        ThreadManager.getInstance().removeHandleTask(mHideSecondaryPoi);
        ThreadManager.getInstance().postDelay(mHideSecondaryPoi, HIDE_DELAY_TIME);
    }

    /***
     * 设置是否存在子POI界面
     * @param show 是否显示子poi界面
     */
    public void setSecondaryPoi(final boolean show) {
        mSecondaryPoi = show;
    }

    /**
     * 修改终点算路请求
     *
     * @param poiInfoEntity 终点数据
     */
    public void requestChangeEnd(final PoiInfoEntity poiInfoEntity) {
        mModel.requestChangeEnd(poiInfoEntity);
        showSecondaryPoi();
    }

    /***
     * 隐藏天气详情
     */
    public void hideWeatherDeatilsUI() {
        removeCurrentPageUI("4");
        mIncludePageVisibility.set(getCurrentPageUI());
    }

    /***
     * 隐藏服务区列表
     */
    public void hideRouteSearchListUI() {
        removeCurrentPageUI("2");
        mIncludePageVisibility.set(getCurrentPageUI());
    }

    /***
     * 展示服务区列表
     * @param poiInfoEntities 列表数据
     */
    public void showRouteSearchListUI(final List<RouteRestAreaDetailsInfo> poiInfoEntities) {
        mCurrentPageHistory.add("2");
        mIncludePageVisibility.set(getCurrentPageUI());
        mView.showRouteSearchListUI(poiInfoEntities);
    }

    /***
     * 隐藏充电列表
     */
    public void hideRouteSearchChargeListUI() {
        removeCurrentPageUI("3");
        mIncludePageVisibility.set(getCurrentPageUI());
    }

    /***
     * 展示列表
     * @param poiInfoEntities 列表数据
     * @param gasChargeAlongList 已添加的数据
     * @param listSearchType 搜索方式
     * @param type 列表类别 0:充电站 1：加油站
     */
    public void showRouteSearchChargeListUI(final List<PoiInfoEntity> poiInfoEntities, final List<RouteParam> gasChargeAlongList
            , final int listSearchType,final int type) {
        if (getCurrentPageUI() != 3) {
            mCurrentPageHistory.add("3");
            mSearchListType = type;
            if (type == 0) {
                mSearchListTitle.set(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge));
            } else if (type == 1) {
                mSearchListTitle.set(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_gas));
            }
            mIncludePageVisibility.set(getCurrentPageUI());
            final RouteLineInfo routeLineInfo = mModel.getSelectLineInfo();
            if (routeLineInfo == null) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "routeLineInfo is null");
                return;
            }
            mRouteChargeTotalMileage.set(ResourceUtils.Companion.getInstance().getString(R.string.route_total_mileage)
                    + routeLineInfo.getMLength());
            final EvRangeOnRouteInfo evRangeOnRouteInfo = mModel.getRangeOnRouteInfo(mModel.getCurrentIndex());
            mRouteTotalDistance = routeLineInfo.getMDistance();
            if (evRangeOnRouteInfo.isMCanArrived()) {
                mRouteProgressChargeExhaustVisibility.set(false);
                mRouteProgressChargeExhaust.set(100);
                mExhaustDistance = mRouteTotalDistance;
                mRouteExhaustDistance = mRouteTotalDistance;
            } else {
                mRouteProgressChargeExhaustVisibility.set(true);
                mExhaustDistance = mRouteTotalDistance - evRangeOnRouteInfo.getMRemainRangeDistance();
                mRouteExhaustDistance = mExhaustDistance;
                final int progress = Math.round(((float) (mExhaustDistance) / mRouteTotalDistance) * 100);
                mRouteProgressChargeExhaust.set(progress);
                mView.updateRouteChargeExhaustUi(progress / 100.0f);
            }

            mView.highlightAlongTab();
        }
        clearRouteChargePoiUi();
        mChargePoiDistanceList.clear();
        mRouteProgressChargeVisibility.set(mView.getEnergyChecked() && listSearchType == 0);
        mAlongTabSearchVisibility.set(listSearchType == 0);
        mView.showRouteSearchChargeListUI(poiInfoEntities, gasChargeAlongList, listSearchType, type);
        if (type == 1) {
            return;
        }
        //初始化进度条扎点
        for (PoiInfoEntity poiInfoEntity : poiInfoEntities) {
            if (isBelongSamePoi(gasChargeAlongList, poiInfoEntity)) {
                addRouteChargePoiUi(poiInfoEntity);
            }
        }
    }

    /***
     * 隐藏服务区详情
     */
    public void hideRouteSearchDetailsUI() {
        removeCurrentPageUI("5");
        mIncludePageVisibility.set(getCurrentPageUI());
    }

    /***
     * 展示服务区详情地址
     * @param address 地址
     */
    public void setDetailsAddress(final String address) {
        mRouteSearchAddress.set(address);
    }

    /***
     * 展示充电列表
     * @param requestPoiInfoEntity 请求的数据
     * @param resultPoiInfoEntity 结果数据
     * @param gasChargeAlongList 保存的列表
     * @param searchType 搜索方式
     */
    public void showRouteSearchDetailsUI(final PoiInfoEntity requestPoiInfoEntity, final PoiInfoEntity resultPoiInfoEntity
            , final List<RouteParam> gasChargeAlongList, final int searchType) {
        mDetailsEntry = requestPoiInfoEntity;
        mDetailsResustEntry = resultPoiInfoEntity;
        mRouteSearchName.set(requestPoiInfoEntity.getName());
        mRouteSearchAddress.set(requestPoiInfoEntity.getAddress());
        mModel.getTravelTimeFutureIncludeChargeLeft(new GeoPoint(requestPoiInfoEntity.getPoint().getLon(),
                        requestPoiInfoEntity.getPoint().getLat()))
                .thenAccept(etaInfo -> {
                    ThreadManager.getInstance().postUi(() -> {
                        mRouteSearchTimeAndDistance.set(MessageFormat.format("{0} {1}",
                                requestPoiInfoEntity.getDistance(), etaInfo.getTravelTime()));
                        mView.showPOIDetailCharge(etaInfo.getLeftCharge());
                        mRouteSearchElec.set(ResourceUtils.Companion.getInstance().getString(
                                com.fy.navi.scene.R.string.remain_charge, etaInfo.getLeftCharge()));
                    });

                })
                .exceptionally(error -> {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "getTravelTimeFuture error:" + error);
                    return null;
                });

        mSeartype = searchType;
        mGasChargeAlongList = gasChargeAlongList;
        if (mSeartype == 0) {
            if (isBelongSamePoi(gasChargeAlongList, requestPoiInfoEntity)) {
                mRouteSearchDeailsAddRemoveVia.set(ResourceUtils.Companion.getInstance().getString(R.string.route_service_details_remove_via));
            } else {
                mRouteSearchDeailsAddRemoveVia.set(ResourceUtils.Companion.getInstance().getString(R.string.route_service_details_add_via));
            }
        } else {
            if (mModel.isBelongRouteParam(requestPoiInfoEntity)) {
                mRouteSearchDeailsAddRemoveVia.set(ResourceUtils.Companion.getInstance().getString(R.string.route_service_details_remove_via));
            } else {
                mRouteSearchDeailsAddRemoveVia.set(ResourceUtils.Companion.getInstance().getString(R.string.route_service_details_add_via));
            }
        }
        if (!ConvertUtils.isEmpty(requestPoiInfoEntity.getServiceAreaInfoList()) && requestPoiInfoEntity.getServiceAreaInfoList().size() > 0
                && !ConvertUtils.isEmpty(requestPoiInfoEntity.getServiceAreaInfoList().get(0))) {
            final ServiceAreaInfo info = requestPoiInfoEntity.getServiceAreaInfoList().get(0);
            if (!ConvertUtils.isEmpty(info.getName())) {
                mRouteSearchName.set(info.getName());
            }
            if (!ConvertUtils.isEmpty(info.getAddress())) {
                mRouteSearchName.set(info.getAddress());
            }
        }
        ThreadManager.getInstance().postUi(() -> {
            final int pointType = SearchPackage.getInstance().getPointTypeCode(requestPoiInfoEntity.getPointTypeCode());
            if (requestPoiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_service))
                    || pointType == AutoMapConstant.PointTypeCode.SERVICE_AREA) {
                mRouteSearchTypeVisibility.set(3);
                mSeartype = 3;
                mView.showServiceDetailsUI(resultPoiInfoEntity);
            } else if (requestPoiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge))
                    || pointType == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
                mRouteSearchTypeVisibility.set(2);
                mView.showChargeDetailsUI(resultPoiInfoEntity);
            } else if (requestPoiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_gas))
                    || pointType == AutoMapConstant.PointTypeCode.GAS_STATION) {
                mRouteSearchTypeVisibility.set(1);
                mView.showPOIDetailGas(resultPoiInfoEntity);
            }
        });
        if (getCurrentPageUI() != 5) {
            mCurrentPageHistory.add("5");
            mIncludePageVisibility.set(getCurrentPageUI());
        }
    }

    /***
     * 判断当前点是否已经在路线上
     * @param local 路径上的点
     * @param poiInfoEntity 当前点
     * @return 是否是属于路径上的点
     */
    private boolean isBelongSamePoi(final List<RouteParam> local, final PoiInfoEntity poiInfoEntity) {
        if (local.isEmpty()) {
            return false;
        }
        for (RouteParam param : local) {
            if (RoutePackage.getInstance().isTheSamePoi(param, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    /***
     * 保存车牌和限行的数据
     */
    public void setDefultPlateNumberAndAvoidLimitSave() {
        mCurrentPlateNumber = mModel.getPlateNumber();
        mCurrentavoidLimit = mModel.getAvoidLimit();
        mCurrentPreferences = mModel.getPreferences();
        mCurrentEnergy = mModel.getEnergy();
    }

    /***
     * 设置补能点开关数据
     */
    public void setCurrentEnergy() {
        mCurrentEnergy = mModel.getEnergy();
        Logger.d(TAG, "mCurrentEnergy: " + mCurrentEnergy);
    }

    /***
     * 设置改变请求重新请求算路
     */
    public void isRequestRouteForPlateNumberAndAvoidLimitChange() {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        if (mCurrentPlateNumber.equals(mModel.getPlateNumber()) && mCurrentavoidLimit.equals(mModel.getAvoidLimit())
                && mCurrentEnergy.equals(mModel.getEnergy()) && mCurrentPreferences.equals(mModel.getPreferences())) {
            return;
        }
        mRestrictionVisibility.set(false);
        if (!mCurrentEnergy.equals(mModel.getEnergy())) {
            setDefultPlateNumberAndAvoidLimitSave();
            mView.setEnergyChecked();
            return;
        }
        setDefultPlateNumberAndAvoidLimitSave();
        final RouteRequestParam param = new RouteRequestParam();
        requestRoute(param);
    }

    /***
     * 限行UI渲染
     * @param routeRestrictionType 限行类型
     */
    public void updateRestrictionTextUI(final int routeRestrictionType) {
        mRestrictionVisibility.set(true);
        mRestrictionRightBackVisibility.set(false);
        mRestrictionStatus = routeRestrictionType;
        mRestrictionBackground.set(ResourceUtils.Companion.getInstance().getDrawable(R.color.text_route_restriction_error));
        mRestrictionTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().getColor(R.color.text_route_restriction_text_error));
        switch (routeRestrictionType) {
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENOPLATE:
                mRestriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstypenoplate));
                mRestrictionRightBackVisibility.set(true);
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENOTOPEN:
                mRestriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstypenotopen));
                mRestrictionRightBackVisibility.set(true);
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEAVOIDSUCCESS:
                mRestriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstypeavoidsuccess));
                mRestrictionBackground.set(ResourceUtils.Companion.getInstance().getDrawable(R.color.text_route_restriction_success));
                mRestrictionTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance()
                        .getColor(R.color.text_route_restriction_text_success));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEREGIONSTART:
                mRestriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstyperegionstart));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEREGIONEND:
                mRestriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstyperegionend));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEREGIONVIA:
                mRestriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstyperegionvia));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEREGIONCROSS:
                mRestriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstyperegioncross));
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEINVALID:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEAVOIDFUTURESUCCESS:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEEXPIREDIMMEDIATELY:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEWAITLIMITOFF:
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEWAITLIMITOFFSHORT:
                Logger.i(TAG, "hide restriction");
                mRestrictionVisibility.set(false);
                break;
            case RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENETWORK:
                mRestriction.set(ResourceUtils.Companion.getInstance().getString(R.string.route_reatirction_limittipstypenetwork));
                break;
            default:
                break;
        }
    }

    /***
     * 选中路线UI
     * @param routeIndex 路线索引
     */
    public void updateSelectRouteUI(final int routeIndex) {
        ThreadManager.getInstance().postUi(() -> mView.updateSelectRouteUI(routeIndex));
    }

    /***
     * 更新长途TAB
     * @param isLongRoute 是否长途路
     */
    public void showHideTab(final boolean isLongRoute) {
        if (mModel.powerType() == 1) {
            mTabVisibility.set(isLongRoute ? 2 : 1);
        } else {
            mTabVisibility.set(isLongRoute ? 2 : 3);
        }
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

    /***
     * 更新长途补能规划
     * @param isLongRoute 是否长途路
     */
    public void showHideElicCheckBox(final boolean isLongRoute) {
        mView.setPreferenceMaxWidth(isLongRoute);
        mBatterCheckBoxVisibility.set(isLongRoute);
    }

    /***
     * 更新充电列表
     * @param gasChargeAlongList 列表数据
     * @param listSearchType 搜索方式
     */
    public void updateChareList(final List<RouteParam> gasChargeAlongList, final int listSearchType) {
        mView.updateChareList(gasChargeAlongList, listSearchType);
    }

    /***
     * 添加推荐充电站
     * @param poiInfoEntity poi数据
     */
    public void addRouteChargePoiUi(final PoiInfoEntity poiInfoEntity) {
        mView.addRouteChargePoiUi(poiInfoEntity, (float) (poiInfoEntity.getSort_distance()) / mRouteTotalDistance);
        mChargePoiDistanceList.add(poiInfoEntity.getSort_distance());
        updateExhaustDistance();

    }

    /***
     * 移除推荐充电站
     * @param poiInfoEntity poi数据
     */
    public void removeRouteChargePoiUi(final PoiInfoEntity poiInfoEntity) {
        mView.removeRouteChargePoiUi(poiInfoEntity);
        final int index = mChargePoiDistanceList.indexOf(poiInfoEntity.getSort_distance());
        if (index == -1) {
            Logger.d(TAG, "mChargePoiDistanceList: " + mChargePoiDistanceList + " distance:" + poiInfoEntity.getSort_distance());
            return;
        }
        mChargePoiDistanceList.remove(index);
        updateExhaustDistance();
    }

    /***
     * 添加充电站内容
     */
    public void updateExhaustDistance() {
        if (mExhaustDistance == mRouteTotalDistance) {
            return;
        }
        mExhaustDistance = mRouteExhaustDistance;
        boolean foundSmaller;
        int farthestDistance = 0;
        do {
            //没有添加充电站
            if (mChargePoiDistanceList == null && mChargePoiDistanceList.isEmpty()) {
                break;
            }
            foundSmaller = false;
            int maxValueSmallerThanX = farthestDistance;

            for (int value : mChargePoiDistanceList) {
                if (value < mExhaustDistance && value > maxValueSmallerThanX) {
                    maxValueSmallerThanX = value;
                }
            }

            if (maxValueSmallerThanX != farthestDistance) {
                mExhaustDistance = (long) (maxValueSmallerThanX + BevPowerCarUtils.getInstance().arrivingPercent
                        * BevPowerCarUtils.getInstance().batterToDistance);
                foundSmaller = true;
            }
            //添加充电站都远于能量耗尽点
            if (maxValueSmallerThanX == 0) {
                mExhaustDistance = mRouteExhaustDistance;
            }
            farthestDistance = maxValueSmallerThanX;

        } while (foundSmaller);

        //算路能耗距离更远
        mExhaustDistance = Math.max(mExhaustDistance, mRouteExhaustDistance);
        mRouteProgressChargeExhaustVisibility.set(true);
        int progress = Math.round(((float) (mExhaustDistance) / mRouteTotalDistance) * 100);
        if (progress >= 100) {
            mRouteProgressChargeExhaustVisibility.set(false);
            progress = 100;
        }
        mRouteProgressChargeExhaust.set(progress);
        mView.updateRouteChargeExhaustUi(progress / 100.0f);
    }

    /***
     * 清除备选充电站数据
     */
    public void clearRouteChargePoiUi() {
        mView.clearRouteChargePoiUi();
    }

    @Override
    public void onRoutePreferenceChange(final String text, final boolean isFirstChange) {
        if (mView.isHidden()) {
            return;
        }
        if (!isFirstChange) {
            final RouteRequestParam param = new RouteRequestParam();
            param.setMRouteWay(RouteWayID.ROUTE_WAY_CHANGE_PREFERENCE);
            param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_STRATEGE);
            requestRoute(param);
            showSecondaryPoi();
        }
        mPreferText.set(text);
        mRoutePreferenceVisibility.set(false);
        mRoutePreferenceDrawableVisibility.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_down));
        //设置偏好设置
        mCurrentPreferences = mModel.getPreferences();
        Logger.d(TAG, "mCurrentPreferences: " + mCurrentPreferences);
    }

    @Override
    public void onResultListUpdate() {
        //路线刷新完毕
    }

    @Override
    public void onRouteSelect(final boolean isTheSameIndex, final int index) {
        cancelTimer();
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        mView.clearSceneTabUI();
        mIsChargingSelect = false;
        mIsGasSelect = false;
        mView.clearSceneTabUI(false);
        mView.clearSceneGasTabUI(false);
        if (isTheSameIndex) {
            mCurrentPageHistory.add("1");
        }
        mIncludePageVisibility.set(getCurrentPageUI());
        mView.setDetailsResult(mModel.getDetailsResult(index));
        mModel.onRouteSelect(index);
    }

    @Override
    public void onListTouch() {
        cancelTimer();
    }

    @Override
    public void onRouteDetailsChecked(final boolean checkedLeastOne) {
        mAvoidBackground.set(checkedLeastOne);
        mAvoidTestColor.set(checkedLeastOne ? ResourceUtils.Companion.getInstance().getColor(R.color.white)
                : ResourceUtils.Companion.getInstance().getColor(R.color.text_route_defult));
        mAvoidClickable.set(checkedLeastOne);
    }

    @Override
    public void onTabListClick(final int tabIndex, final boolean isChecked) {
        cancelTimer();
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        switch (tabIndex) {
            case 0:
                if (isChecked) {
                    mModel.getSearchListChargeAndShow(mSearchKeyWord, 0);
                } else {
                    hideRouteSearchDetailsUI();
                    hideRouteSearchListUI();
                    hideRouteSearchChargeListUI();
                    mModel.clearSearchLabel();
                }
                break;
            case 1:
                if (isChecked) {
                    mModel.getWeatherList();
                } else {
                    mModel.hideWeatherList();
                }
                break;
            case 2:
                if (isChecked) {
                    mModel.getSearchListAndShow();
                } else {
                    hideRouteSearchDetailsUI();
                    hideRouteSearchListUI();
                    hideRouteSearchChargeListUI();
                    mModel.clearRestArea();
                }
                break;
            case 3:
                if (isChecked) {
                    mModel.getSearchListChargeAndShow(mGasSearchKeyWord, 0);
                } else {
                    hideRouteSearchDetailsUI();
                    hideRouteSearchListUI();
                    hideRouteSearchChargeListUI();
                    mModel.clearSearchLabel();
                }
                break;
            default:
                break;
        }
    }

    @Override
    public void onTabListGasChargeClick(final int tabIndex) {
        if (mSearchListType == 0) {
            mModel.getSearchListChargeAndShow(mSearchKeyWord, tabIndex);
        } else if (mSearchListType == 1) {
            mModel.getSearchListChargeAndShow(mGasSearchKeyWord, tabIndex);
        }
    }

    private boolean mIsChargingSelect = false;
    private Action mTabChargingClick = () -> {
        cancelTimer();
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        if (Boolean.FALSE.equals(mIsChargingSelect)) {
            mModel.getSearchListChargeAndShow(mSearchKeyWord, 0);
        } else {
            hideRouteSearchDetailsUI();
            hideRouteSearchListUI();
            mModel.clearSearchLabel();
        }
        mIsChargingSelect = !mIsChargingSelect;
        mView.clearSceneTabUI(mIsChargingSelect);
    };

    public Action getTabChargingClick() {
        return mTabChargingClick;
    }

    private boolean mIsGasSelect = false;
    private Action mTabGasClick = () -> {
        cancelTimer();
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        if (Boolean.FALSE.equals(mIsGasSelect)) {
            mModel.getSearchListChargeAndShow(mGasSearchKeyWord, 0);
        } else {
            hideRouteSearchDetailsUI();
            hideRouteSearchListUI();
            mModel.clearSearchLabel();
        }
        mIsGasSelect = !mIsGasSelect;
        mView.clearSceneGasTabUI(mIsGasSelect);
    };

    public Action getTabGasClick() {
        return mTabGasClick;
    }

    @Override
    public void enterToDetails(final PoiInfoEntity poiInfoEntity) {
        mModel.getSearchDetailsMode(poiInfoEntity);
    }

    @Override
    public boolean onTouch(final View view, final MotionEvent motionEvent) {
        cancelTimer();
        return false;
    }

    @Override
    public void enterToChargeDetails(final PoiInfoEntity poiInfoEntity) {
        mModel.getSearchDetailsMode(poiInfoEntity);
    }

    @Override
    public void onGasChargeRemoveClick(final PoiInfoEntity poiInfoEntity) {
        mModel.gasChargeRemoveMode(poiInfoEntity);
    }

    @Override
    public void onGasChargeAddClick(final PoiInfoEntity poiInfoEntity) {
        mModel.gasChargeAddMode(poiInfoEntity);
    }

    @Override
    public void onClose() {
        mModel.cancelRoute();
    }

    public void onReStoreFragment() {
        showNomalRouteUI();
        mModel.onReStoreFragment();
        if (mSecondaryPoiInfo != null) {
            mView.setRouteSecondaryPoiUI(mSecondaryPoiInfo);
        }
    }
}
