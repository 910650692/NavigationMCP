package com.fy.navi.hmi.route;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.constraintlayout.widget.ConstraintSet;
import androidx.recyclerview.widget.ItemTouchHelper;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.recyclerview.widget.GridLayoutManager;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentRouteBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.adapter.GasStationAdapter;
import com.fy.navi.scene.adapter.GridSpacingItemDecoration;
import com.fy.navi.scene.dialog.MsgTopDialog;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.adapter.RoutePOIGasStationAdapter;
import com.fy.navi.scene.ui.adapter.RoutePOIIconAdapter;
import com.fy.navi.scene.ui.adapter.RouteSecondaryPoiAdapter;
import com.fy.navi.scene.ui.adapter.RouteViaPointAdapter;
import com.fy.navi.scene.ui.search.RouteRequestLoadingDialog;
import com.fy.navi.scene.ui.search.RouteSearchLoadingDialog;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ServiceAreaInfo;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.ui.define.TripID;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;
import com.fy.navi.ui.view.SkinConstraintLayout;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Route(path = RoutePath.Route.ROUTE_FRAGMENT)
public class RouteFragment extends BaseFragment<FragmentRouteBinding, RouteViewModel> {
    private static final String TAG = RouteFragment.class.getSimpleName();
    private final int mSpacing = 24; // 上下间距
    private final int mHorizontalSpacing = 32; // 左右间距
    private final int mSpanCount = 2;//数据列数
    private static final int SWIPE_THRESHOLD = 100;
    private RouteRequestLoadingDialog mRouteRequestLoadingDialog;
    private RouteSearchLoadingDialog mSearchLoadingDialog;
    private RouteViaPointAdapter mRouteViaPointAdapter;
    private RoutePOIIconAdapter mPoiIconAdapter;
    private RoutePOIGasStationAdapter mGasStationAdapter;
    private RouteSecondaryPoiAdapter mRouteSecondaryPoiAdapter;
    private MsgTopDialog mMsgTopDialog;
    private Map<PoiInfoEntity, View> mRouteChargeProgressViews;
    private float mStartY;
    private float mEndY;

    private final static String ROUTE_ERROR = "异常";

    @Override
    public int onLayoutId() {
        return R.layout.fragment_route;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        inItCheckBox();
        initViaPoiAdaper();
        initDetailsAdaper();
        initRouteScene();
        initTouchCloseTimer();
        initTouchListener();
        initCarType();
    }

    /***
     * 补能规划
     */
    private void inItCheckBox() {
        mBinding.routeLineInfoSwitchEnergy.setChecked(SettingPackage.getInstance().getChargingPlan());
        mBinding.routeLineInfoSwitchEnergy.setOnCheckedChangeListener((compoundButton, b) -> {
            mViewModel.cancelTimer();
            if (b) {
                hideTrip();
            }
            BevPowerCarUtils.getInstance().isElecPlanRoute = b;
            SettingPackage.getInstance().setChargingPlan(b);
            final RouteRequestParam param = new RouteRequestParam();
            param.setMRouteWay(RouteWayID.ROUTE_WAY_REFRESH);
            param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH);
            mViewModel.setCurrentEnergy();
            mViewModel.requestRoute(param);
        });
        BevPowerCarUtils.getInstance().isElecPlanRoute = mBinding.routeLineInfoSwitchEnergy.isChecked();
    }

    /***
     * 路线途经点列表
     */
    private void initViaPoiAdaper() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.routeLineViaPoiRecycle.setLayoutManager(layoutManager);
        mRouteViaPointAdapter = new RouteViaPointAdapter();
        mRouteViaPointAdapter.setDeleteViaPointListener(index -> {
            mViewModel.cancelTimer();
            mViewModel.deleteViaParamMode(index);
        });
        mBinding.routeLineViaPoiRecycle.setAdapter(mRouteViaPointAdapter);
        final ItemTouchHelper touchHelper = new ItemTouchHelper(new ItemTouchHelper.Callback() {
            private int movePosition = -1;
            private int mCurrentPosition = -1;

            @Override
            public int getMovementFlags(final RecyclerView recyclerView, final RecyclerView.ViewHolder viewHolder) {
                final int dragFlags = ItemTouchHelper.UP | ItemTouchHelper.DOWN;
                final int swipeFlags = ItemTouchHelper.START | ItemTouchHelper.END;
                return makeMovementFlags(dragFlags, swipeFlags);
            }

            @Override
            public boolean onMove(final RecyclerView recyclerView, final RecyclerView.ViewHolder viewHolder
                    , final RecyclerView.ViewHolder target) {
                mCurrentPosition = viewHolder.getAdapterPosition();
                movePosition = target.getAdapterPosition();
                mRouteViaPointAdapter.onItemMove(viewHolder.getAdapterPosition(), target.getAdapterPosition());
                return true;
            }

            @Override
            public void onSwiped(final RecyclerView.ViewHolder viewHolder, final int direction) {
            }

            @Override
            public void clearView(final RecyclerView recyclerView, final RecyclerView.ViewHolder viewHolder) {
                super.clearView(recyclerView, viewHolder);
                mViewModel.changeParamListMode(mCurrentPosition, movePosition);
            }

            @Override
            public boolean isItemViewSwipeEnabled() {
                return false;
            }
        });
        touchHelper.attachToRecyclerView(mBinding.routeLineViaPoiRecycle);
    }

    /***
     * 路线导航段列表
     */
    private void initDetailsAdaper() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.HORIZONTAL);
        mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setLayoutManager(layoutManager);
        mPoiIconAdapter = new RoutePOIIconAdapter();
        mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setAdapter(mPoiIconAdapter);

        final LinearLayoutManager layoutManager1 = new LinearLayoutManager(getContext());
        layoutManager1.setOrientation(LinearLayoutManager.HORIZONTAL);
        mBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.setLayoutManager(layoutManager1);
        mGasStationAdapter = new RoutePOIGasStationAdapter();
        mBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.setAdapter(mGasStationAdapter);

        final LinearLayoutManager layoutManager2 = new LinearLayoutManager(getContext());
        layoutManager2.setOrientation(LinearLayoutManager.HORIZONTAL);
        mBinding.rvSecondaryPoi.setLayoutManager(layoutManager2);
        mRouteSecondaryPoiAdapter = new RouteSecondaryPoiAdapter();
        mBinding.rvSecondaryPoi.setAdapter(mRouteSecondaryPoiAdapter);
        mRouteSecondaryPoiAdapter.setItemClickListener(new RouteSecondaryPoiAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final ChildInfo childInfo) {
                mViewModel.cancelTimer();
                final PoiInfoEntity poiInfo = new PoiInfoEntity();
                poiInfo.setPid(childInfo.getPoiId());
                poiInfo.setName(childInfo.getName());
                poiInfo.setAddress(childInfo.getAddress());
                poiInfo.setPoint(new GeoPoint(childInfo.getLocation().getLon(), childInfo.getLocation().getLat()));
                mViewModel.requestChangeEnd(poiInfo);
            }

            @Override
            public void onCancelSelectClick(final PoiInfoEntity poiInfoEntity) {
                mViewModel.cancelTimer();
                mViewModel.requestChangeEnd(poiInfoEntity);
            }
        });
        mBinding.rvSecondaryPoi.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrollStateChanged(@NonNull final RecyclerView recyclerView, final int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                mViewModel.cancelTimer();
            }

            @Override
            public void onScrolled(@NonNull final RecyclerView recyclerView, final int dx, final int dy) {
                super.onScrolled(recyclerView, dx, dy);
            }
        });
    }

    /***
     * Scene 初始化
     */
    private void initRouteScene() {
        mBinding.routeLineInfoSceneRouteResult.setScreenId(MapType.valueOf(mScreenId));
        mBinding.routeDetailInfoSceneRouteDetailsResult.setScreenId(MapType.valueOf(mScreenId));
        mBinding.routeLineInfoSceneRoutePerference.setScreenId(MapType.valueOf(mScreenId));
        mBinding.routeRightTabListScene.setScreenId(MapType.valueOf(mScreenId));
        mBinding.routeRightTabListChargeScene.setScreenId(MapType.valueOf(mScreenId));
        mBinding.routeDetailInfoSceneRouteSearchRefresh.setScreenId(MapType.valueOf(mScreenId));
        mBinding.routeChargeInfoSceneRouteSearchRefresh.setScreenId(MapType.valueOf(mScreenId));
        mBinding.routeLineInfoSceneRoutePerference.registerRoutePreferenceObserver(TAG, mViewModel);
        mBinding.routeLineInfoSceneRouteResult.registerRouteSelectObserver(TAG, mViewModel);
        mBinding.routeDetailInfoSceneRouteDetailsResult.registerRouteDeatailsCheckedObserver(TAG, mViewModel);
        mBinding.routeRightTabListScene.registerRouteSelectObserver(TAG, mViewModel);
        mBinding.routeRightTabListChargeScene.registerRouteSelectObserver(TAG, mViewModel);
        mBinding.routeDetailInfoSceneRouteSearchRefresh.registerRouteSearchRefreshObserver(TAG, mViewModel);
        mBinding.routeChargeInfoSceneRouteSearchRefresh.registerRouteSearchRefreshObserver(TAG, mViewModel);
        mBinding.scenePoiDetailsGasStationView.poiGasOilList.addItemDecoration(
                new GridSpacingItemDecoration(getContext(), mSpanCount, mSpacing, mHorizontalSpacing, false));
    }

    /***
     * Touch 监听
     */
    @SuppressLint("ClickableViewAccessibility")
    private void initTouchCloseTimer() {
        mBinding.routeRightTabListScene.setOnTouchListener(mViewModel);
        mBinding.routeLineInfoRoot.setOnTouchListener(mViewModel);
        mBinding.routeDetailInfoRoot.setOnTouchListener(mViewModel);
        mBinding.routeLineViaPoiRecycle.setOnTouchListener(mViewModel);
    }

    /***
     * 设置点击监听
     */
    @SuppressLint("ClickableViewAccessibility")
    private void initTouchListener() {
        mBinding.routeLineInfoTitle.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(final View v, final MotionEvent event) {
                switch (event.getAction()) {
                    case MotionEvent.ACTION_DOWN:
                        mStartY = event.getY();
                        break;
                    case MotionEvent.ACTION_UP:
                        mEndY = event.getY();
                        final float diffY = mEndY - mStartY;
                        if (Math.abs(diffY) > SWIPE_THRESHOLD) {
                            mViewModel.getOpenCloseViaClick().call();
                        }
                        break;
                    default:
                        break;
                }
                return true;
            }
        });
    }

    /***
     * 适配车辆类型
     */
    private void initCarType() {
        mBinding.routeRightTabListScene.setCarType(mViewModel.powerType());
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog)) {
            mRouteRequestLoadingDialog.dismiss();
            mRouteRequestLoadingDialog = null;
        }

        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.dismiss();
            mSearchLoadingDialog = null;
        }
    }

    @Override
    public void onInitData() {
        mViewModel.setDefultPlateNumberAndAvoidLimitSave();
        final Bundle bundle = getArguments();
        assert bundle != null;
        final RouteSpeechRequestParam param = (RouteSpeechRequestParam) bundle.getSerializable("speech_open_route");
        if (!ConvertUtils.isEmpty(param)) {
            Logger.i(TAG, "speech: " + ConvertUtils.isEmpty(param));
            mViewModel.requestRouteFromSpeech(param);
            return;
        }

        final RouteMsgPushInfo routeMsgPushInfo = (RouteMsgPushInfo) bundle.getSerializable(
                AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MSG_PUSH_OPEN_ROUTE_TYPE);
        if (!ConvertUtils.isEmpty(routeMsgPushInfo)) {
            Logger.i(TAG, "nomal: " + GsonUtils.toJson(routeMsgPushInfo));
            mViewModel.getTitle().set(routeMsgPushInfo.getMName());
            mViewModel.getEndName().set(routeMsgPushInfo.getMName());
            mBinding.routeDetailInfoSceneRouteDetailsResult.setEndPoint(routeMsgPushInfo.getMName());
            mViewModel.requestRouteRestoration(routeMsgPushInfo);
            return;
        }

        final PoiInfoEntity poiInfoEntity = (PoiInfoEntity) bundle.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE);
        final int poiType = (int) bundle.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE);
        if (!ConvertUtils.isEmpty(poiInfoEntity)) {
            Logger.i(TAG, "nomal: " + ConvertUtils.isEmpty(poiInfoEntity));
            mViewModel.getTitle().set(poiInfoEntity.getName());
            mViewModel.getEndName().set(poiInfoEntity.getName());
            mBinding.routeDetailInfoSceneRouteDetailsResult.setEndPoint(poiInfoEntity.getName());
            final RouteRequestParam routeRequestParam = new RouteRequestParam();
            routeRequestParam.setMPoiInfoEntity(poiInfoEntity);
            routeRequestParam.setMRoutePoiType(poiType);
            mViewModel.requestRoute(routeRequestParam);

            //子poi显示
            if (poiInfoEntity.getChildInfoList() != null && !poiInfoEntity.getChildInfoList().isEmpty()) {
                mRouteSecondaryPoiAdapter.setChildInfoList(poiInfoEntity.getChildInfoList(), poiInfoEntity);
                mViewModel.setSecondaryPoi(true);
                mViewModel.showSecondaryPoi();
            }
            return;
        }
        closeFragment(true);
        ToastUtils.Companion.getInstance().showCustomToastView(
                ResourceUtils.Companion.getInstance().getString(R.string.route_error_no_request_data));
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            //设置全览态
            ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.isRequestRouteForPlateNumberAndAvoidLimitChange();
            } else {
                Logger.e(TAG, ROUTE_ERROR);
            }
            if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeLineInfoSceneRoutePerference)) {
                mBinding.routeLineInfoSceneRoutePerference.resetPreference();
            } else {
                Logger.e(TAG, ROUTE_ERROR);
            }
        }
    }

    /***
     * 设置规避UI
     * @param isAvoid true
     */
    public void setAvoidStatusUI(final boolean isAvoid) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeDetailInfoSceneRouteDetailsResult)) {
            mBinding.routeDetailInfoSceneRouteDetailsResult.setAvoidStatus(isAvoid);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 点击规避
     */
    public void startAvoidRoad() {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.getRouteDetailsVisibility().set(true);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
        setAvoidStatusUI(false);
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeDetailInfoSceneRouteDetailsResult)) {
            mBinding.routeDetailInfoSceneRouteDetailsResult.startAvoidRoad();
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 渲染算路结果列表
     * @param routeLineInfos 数据
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_ROUTE_LIST)
    public void setRouteResultListUI(final List<RouteLineInfo> routeLineInfos) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeLineInfoSceneRouteResult)) {
            mBinding.routeLineInfoSceneRouteResult.notifyResultList(routeLineInfos);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }

        //For Bury Point
        final BuryProperty buryProperty = new BuryProperty.Builder().setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION,
                Integer.toString(routeLineInfos.size())).build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /***
     * 渲染导航段
     * @param routeLineSegmentInfos 数据
     */
    public void setDetailsResult(final List<RouteLineSegmentInfo> routeLineSegmentInfos) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeDetailInfoSceneRouteDetailsResult)) {
            mBinding.routeDetailInfoSceneRouteDetailsResult.notifyRouteDetailsResultList(routeLineSegmentInfos);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 设置补能规划开关
     */
    public void setEnergyChecked() {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeLineInfoSwitchEnergy)) {
            mBinding.routeLineInfoSwitchEnergy.setChecked(!mBinding.routeLineInfoSwitchEnergy.isChecked());
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 获取补能规划开关
     *
     * @return 开关状态
     */
    public boolean getEnergyChecked() {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeLineInfoSwitchEnergy)) {
            return !mBinding.routeLineInfoSwitchEnergy.isChecked() &&
                    mBinding.routeLineInfoSwitchEnergy.getVisibility() == View.VISIBLE;
        } else {
            Logger.e(TAG, ROUTE_ERROR);
            return false;
        }
    }

    /***
     * 算路请求弹框展示
     */
    public void showProgressUI() {
        mRouteRequestLoadingDialog = new RouteRequestLoadingDialog(
                StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()));
        mRouteRequestLoadingDialog.setOnCloseClickListener(mViewModel);
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog)) {
            mRouteRequestLoadingDialog.show();
        }
    }

    /***
     * 算路请求弹框关闭
     */
    public void hideProgressUI() {
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog)) {
            mRouteRequestLoadingDialog.dismiss();
            mRouteRequestLoadingDialog = null;
        }
    }

    /***
     * 搜索请求弹框开启
     */
    public void showSearchProgressUI() {
        mSearchLoadingDialog = new RouteSearchLoadingDialog(
                StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()));
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.show();
        }
    }

    /***
     * 搜索请求弹框关闭
     */
    public void hideSearchProgressUI() {
        if (!ConvertUtils.isEmpty(mSearchLoadingDialog)) {
            mSearchLoadingDialog.dismiss();
            mSearchLoadingDialog = null;
        }
    }

    /***
     * 开启Trip弹框
     * @param title
     * @param content
     */
    public void showTripDialog(final String title, final String content) {
        if (ConvertUtils.isEmpty(mBinding) || ConvertUtils.isEmpty(mBinding.routeLineInfoSwitchEnergy)
                || ConvertUtils.isEmpty(mViewModel)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        if (mBinding.routeLineInfoSwitchEnergy.isChecked()) {
            return;
        }
        if (!ConvertUtils.isEmpty(mMsgTopDialog) && mMsgTopDialog.isShowing()) {
            return;
        }
        mViewModel.cancelTimer();
        mMsgTopDialog = new MsgTopDialog(
                StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()), TripID.ROUTE_LOW_BATTER);
        mMsgTopDialog.setTitle(title);
        mMsgTopDialog.setContent(content);
        mMsgTopDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick(final TripID tripID) {
                if (tripID == TripID.ROUTE_LOW_BATTER) {
                    mBinding.routeLineInfoSwitchEnergy.setChecked(true);
                }
            }
        });
        mMsgTopDialog.showDialog();
    }

    /***
     * 关闭Trip弹框
     */
    public void hideTrip() {
        if (!ConvertUtils.isEmpty(mMsgTopDialog)) {
            mMsgTopDialog.cancel();
            mMsgTopDialog = null;
        }
    }

    /***
     * 设置途经点列表
     * @param routeParams 数据
     */
    public void setViaList(final List<RouteParam> routeParams) {
        if (!ConvertUtils.isEmpty(mRouteViaPointAdapter)) {
            mRouteViaPointAdapter.setRouteBeanList(routeParams);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 去除Tab选中
     */
    public void clearSceneTabUI() {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeRightTabListScene)) {
            mBinding.routeRightTabListScene.clearSceneTabUI();
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 去除Tab选中
     * @param isCharging 是否是充电
     */
    public void clearSceneTabUI(final boolean isCharging) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeRightTabListTvChargingStation)
                && !ConvertUtils.isEmpty(mBinding.routeRightTabListIvChargingStation)) {
            mBinding.routeRightTabListTvChargingStation.setSelected(isCharging);
            mBinding.routeRightTabListIvChargingStation.setSelected(isCharging);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 去除GasTab选中
     * @param isGas 是否是加油
     */
    public void clearSceneGasTabUI(final boolean isGas) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeRightTabListTvGasStation)
                && !ConvertUtils.isEmpty(mBinding.routeRightTabListIvGasStation)) {
            mBinding.routeRightTabListTvGasStation.setSelected(isGas);
            mBinding.routeRightTabListIvGasStation.setSelected(isGas);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 展示服务区列表
     * @param poiInfoEntities 数据
     */
    public void showRouteSearchListUI(final List<RouteRestAreaDetailsInfo> poiInfoEntities) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeDetailInfoSceneRouteSearchRefresh)) {
            mBinding.routeDetailInfoSceneRouteSearchRefresh.notifyResultList(poiInfoEntities);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 展示充电站列表
     * @param poiInfoEntities 搜索数据
     * @param gasChargeAlongList 本地已添加数据
     * @param searchType 搜索方式 0-沿途搜索
     * @param type 列表类别 0:充电站 1：加油站
     */
    public void showRouteSearchChargeListUI(final List<PoiInfoEntity> poiInfoEntities
            , final List<RouteParam> gasChargeAlongList, final int searchType, final int type) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeChargeInfoSceneRouteSearchRefresh)) {
            mBinding.routeChargeInfoSceneRouteSearchRefresh.notifyResultList(poiInfoEntities, gasChargeAlongList, searchType, type);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 高亮沿途按钮
     */
    public void highlightAlongTab() {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeRightTabListChargeScene)) {
            mBinding.routeRightTabListChargeScene.highlightAlongTab();
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 更新充电本地数据
     * @param gasChargeAlongList 本地数据
     * @param listSearchType 搜索方式
     */
    public void updateChareList(final List<RouteParam> gasChargeAlongList, final int listSearchType) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeChargeInfoSceneRouteSearchRefresh)) {
            mBinding.routeChargeInfoSceneRouteSearchRefresh.updateChargeList(gasChargeAlongList, listSearchType);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 更新选中路线
     * @param routeIndex 选中路线索引
     */
    public void updateSelectRouteUI(final int routeIndex) {
        if (!ConvertUtils.isEmpty(mBinding) && !ConvertUtils.isEmpty(mBinding.routeLineInfoSceneRouteResult)) {
            mBinding.routeLineInfoSceneRouteResult.updateSelectRouteUI(routeIndex);
        } else {
            Logger.e(TAG, ROUTE_ERROR);
        }
    }

    /***
     * 更新路径
     * @param progress 百分比
     */
    public void updateRouteChargeExhaustUi(final float progress) {
        if (ConvertUtils.isEmpty(mBinding) || ConvertUtils.isEmpty(mBinding.routeChargeExhaustionPoint)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            final ConstraintLayout.LayoutParams layoutParams = (ConstraintLayout.LayoutParams) mBinding.routeChargeExhaustionPoint.getLayoutParams();
            layoutParams.horizontalBias = progress;
            mBinding.routeChargeExhaustionPoint.setLayoutParams(layoutParams);
        });
    }

    /***
     * 设置充电UI
     * @param poiInfoEntity POI数据
     * @param progress 百分比
     */
    public void addRouteChargePoiUi(final PoiInfoEntity poiInfoEntity, final float progress) {
        if (ConvertUtils.isEmpty(mBinding) || ConvertUtils.isEmpty(mBinding.routeChargeProgressIcons)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            final SkinConstraintLayout routeChargeProgressLayout = mBinding.routeChargeProgressIcons;
            final LayoutInflater inflater = getLayoutInflater();
            final View customViewItem = inflater.inflate(R.layout.item_route_charge_progress, routeChargeProgressLayout, false);
            customViewItem.setId(View.generateViewId());
            final SkinTextView distanceText = customViewItem.findViewById(R.id.tv_route_charge);
            distanceText.setText(poiInfoEntity.getDistance());
            if (mRouteChargeProgressViews == null) {
                mRouteChargeProgressViews = new ConcurrentHashMap<>();
            }
            routeChargeProgressLayout.addView(customViewItem);

            final ConstraintSet constraintSet = new ConstraintSet();
            constraintSet.clone(routeChargeProgressLayout);
            constraintSet.connect(customViewItem.getId(), ConstraintSet.START, routeChargeProgressLayout.getId(), ConstraintSet.START);
            constraintSet.connect(customViewItem.getId(), ConstraintSet.END, routeChargeProgressLayout.getId(), ConstraintSet.END);
            constraintSet.connect(customViewItem.getId(), ConstraintSet.TOP, routeChargeProgressLayout.getId(), ConstraintSet.TOP);
            constraintSet.setHorizontalBias(customViewItem.getId(), progress);
            constraintSet.applyTo(routeChargeProgressLayout);
            mRouteChargeProgressViews.put(poiInfoEntity, customViewItem);

        });

    }

    /***
     * 移除充电UI
     * @param poiInfoEntity POI数据
     */
    public void removeRouteChargePoiUi(final PoiInfoEntity poiInfoEntity) {
        ThreadManager.getInstance().postUi(() -> {
            if (mRouteChargeProgressViews != null) {
                final View view = mRouteChargeProgressViews.get(poiInfoEntity);
                if (view != null) {
                    mBinding.routeChargeProgressIcons.removeView(view);
                }
                mRouteChargeProgressViews.remove(poiInfoEntity);
            }
        });
    }

    /***
     * 清除所有充电UI
     */
    public void clearRouteChargePoiUi() {
        ThreadManager.getInstance().postUi(() -> {
            if (mRouteChargeProgressViews != null && !mRouteChargeProgressViews.isEmpty()) {
                for (PoiInfoEntity poiInfoEntity : mRouteChargeProgressViews.keySet()) {
                    final View view = mRouteChargeProgressViews.get(poiInfoEntity);
                    if (view != null && !ConvertUtils.isEmpty(mBinding)
                            && !ConvertUtils.isEmpty(mBinding.routeChargeProgressIcons)) {
                        mBinding.routeChargeProgressIcons.removeView(view);
                    }
                    mRouteChargeProgressViews.remove(poiInfoEntity);
                }
            }
        });
    }

    /***
     * 展示服务区POI详情数据
     * @param info POI数据
     */
    @SuppressLint("SetTextI18n")
    public void showServiceDetailsUI(final PoiInfoEntity info) {
        if (ConvertUtils.isEmpty(info) || ConvertUtils.isEmpty(mBinding)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsServiceAreaView)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil)
                || ConvertUtils.isEmpty(mBinding.stlPhone)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        if (ConvertUtils.isEmpty(info.getPhone())) {
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setVisibility(View.GONE);
            mBinding.stlPhone.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setVisibility(View.VISIBLE);
            mBinding.stlPhone.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + info.getPhone());
        }

        if (ConvertUtils.isEmpty(info.getBusinessTime())) {
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_bussTime) + info.getBusinessTime());
        }

        if (!ConvertUtils.isEmpty(info.getServiceAreaInfoList())
                && !info.getServiceAreaInfoList().isEmpty() && !ConvertUtils.isEmpty(info.getServiceAreaInfoList().get(0))
                && !ConvertUtils.isEmpty(mViewModel)) {
            final List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildList = info.getServiceAreaInfoList().get(0).getServiceAreaChildList();
            final int building = info.getServiceAreaInfoList().get(0).getBuilding();
            mViewModel.getRouteSearchStatusVisibility().set(true);
            switch (building) {
                case 1:
                    mViewModel.getRouteSearchStatus().set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_building));
                    break;
                case 2:
                    mViewModel.getRouteSearchStatus().set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_not_find));
                    break;
                case 3:
                    mViewModel.getRouteSearchStatus().set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_starting));
                    break;
                case 4:
                    mViewModel.getRouteSearchStatus().set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_stoped));
                    break;
                default:
                    mViewModel.getRouteSearchStatusVisibility().set(false);
                    break;
            }
            if (!serviceAreaChildList.isEmpty()) {
                mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setVisibility(View.VISIBLE);
                final List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildArrayList = new ArrayList<>();
                final List<String> codes = new ArrayList<>();
                for (ServiceAreaInfo.ServiceAreaChild child : serviceAreaChildList) {
                    if (!codes.contains(child.getTypeCode())
                            && isNeed(child.getTypeCode())) {
                        codes.add(child.getTypeCode());
                        serviceAreaChildArrayList.add(child);
                    }
                }
                if (!ConvertUtils.isEmpty(mPoiIconAdapter)) {
                    mPoiIconAdapter.setRouteBeanList(serviceAreaChildArrayList);
                }
                String gasType = "";
                for (ServiceAreaInfo.ServiceAreaChild child : serviceAreaChildArrayList) {
                    if (!ConvertUtils.isEmpty(child.getGasType())) {
                        gasType = child.getGasType();
                    }
                }
                if (ConvertUtils.isEmpty(gasType)) {
                    mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(View.GONE);
                } else {
                    final List<String> gasString = new ArrayList<>();
                    if (gasType.contains("|")) {
                        final String[] split = gasType.split("\\|");
                        gasString.addAll(Arrays.asList(split));
                    } else {
                        gasString.add(gasType);
                    }
                    if (!ConvertUtils.isEmpty(mGasStationAdapter)) {
                        mGasStationAdapter.setRouteBeanList(gasString);
                    }
                    mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(View.VISIBLE);
                }
            } else {
                mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setVisibility(View.GONE);
                mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(View.GONE);
            }
        }
    }

    /***
     * 需要展示的设施
     * @param typeCode 设置code
     * @return 需要展示的设置code
     */
    private boolean isNeed(final String typeCode) {
        return "010200".equals(typeCode) || "200300".equals(typeCode)
                || "060400".equals(typeCode) || "150904".equals(typeCode)
                || "011100".equals(typeCode) || "010000".equals(typeCode)
                || "050100".equals(typeCode);
    }

    /***
     * 展示充电站POI详情数据
     * @param info POI数据
     */
    @SuppressLint("SetTextI18n")
    public void showChargeDetailsUI(final PoiInfoEntity info) {
        mBinding.sivArrivalCapacity.setVisibility(View.VISIBLE);
        mBinding.poiArrivalCapacity.setVisibility(View.VISIBLE);
        if (ConvertUtils.isEmpty(info) || ConvertUtils.isEmpty(mBinding)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeImg)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargePrice)
                || ConvertUtils.isEmpty(mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice)
                || ConvertUtils.isEmpty(mBinding.stlPhone)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        ViewAdapterKt.loadImageUrl(mBinding.scenePoiDetailsChargingStationView.poiChargeImg
                , info.getImageUrl(), com.fy.navi.scene.R.drawable.test_pic, com.fy.navi.scene.R.drawable.test_pic);
        if (ConvertUtils.isEmpty(info.getPhone())) {
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.GONE);
            mBinding.stlPhone.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.VISIBLE);
            mBinding.stlPhone.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + info.getPhone());
        }

        if (ConvertUtils.isEmpty(info.getBusinessTime())) {
            mBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_bussTime) + info.getBusinessTime());
        }
        if (!ConvertUtils.isEmpty(info.getChargeInfoList()) && !info.getChargeInfoList().isEmpty()) {
            final ChargeInfo chargeInfo = info.getChargeInfoList().get(0);
            mBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied.setText(chargeInfo.getFast_free() + "");
            mBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getFast_free());
            mBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot
                    .setText(chargeInfo.getFastPower() + "kw." + chargeInfo.getFastVolt() + "v");

            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(chargeInfo.getSlow_free() + "");
            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getSlow_total());
            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot
                    .setText(chargeInfo.getSlowPower() + "kw." + chargeInfo.getSlowVolt() + "v");

            mBinding.scenePoiDetailsChargingStationView.poiChargePrice
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free)
                            + chargeInfo.getCurrentElePrice()
                            + ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free_unit));
            mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_park_free)
                            + chargeInfo.getCurrentServicePrice());
        }
    }

    /***
     * 展示POI详情的剩余电量数据
     * @param leftCharge 剩余电量
     */
    public void showPOIDetailCharge(final int leftCharge) {
        if (ConvertUtils.isEmpty(mBinding)
                || ConvertUtils.isEmpty(mBinding.poiArrivalCapacity)
                || ConvertUtils.isEmpty(mBinding.sivArrivalCapacity)) {
            Logger.e(TAG, ROUTE_ERROR);
            return;
        }
        if (!ConvertUtils.isEmpty(leftCharge)) {
            //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
            //0-20电量，显示低电量图片，文本变红
            //小于0%电量，显示空电量图片，文本变红
            if (leftCharge >= 50 && leftCharge <= 100) {
                mBinding.sivArrivalCapacity.setImageResource(com.fy.navi.scene.R.drawable.img_electricity_full_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.fy.navi.scene.R.color.text_color_route_item_select));
            } else if (leftCharge > 20 && leftCharge < 50) {
                mBinding.sivArrivalCapacity.setImageResource(com.fy.navi.scene.R.drawable.img_electricity_medium_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.fy.navi.scene.R.color.text_color_route_item_select));
            } else if (leftCharge > 0 && leftCharge <= 20) {
                mBinding.sivArrivalCapacity.setImageResource(com.fy.navi.scene.R.drawable.img_electricity_low_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.fy.navi.scene.R.color.search_color_delete_bg));
            } else if (leftCharge <= 0) {
                mBinding.sivArrivalCapacity.setImageResource(com.fy.navi.scene.R.drawable.img_electricity_empty_42);
                mBinding.poiArrivalCapacity.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(com.fy.navi.scene.R.color.search_color_delete_bg));
            }
        }
    }

    /***
     * 展示POI详情的加油站数据
     * @param info POI数据
     */
    public void showPOIDetailGas(final PoiInfoEntity info) {
        mBinding.sivArrivalCapacity.setVisibility(View.GONE);
        mBinding.poiArrivalCapacity.setVisibility(View.GONE);
        final List<GasStationInfo> gasStationInfos = info.getStationList();
        for (GasStationInfo gasStationInfo : gasStationInfos) {
            gasStationInfo.setPrice(getContext().getString(R.string.route_oil_price, gasStationInfo.getPrice()));
        }
        final GasStationAdapter gasStationAdapter = new GasStationAdapter();
        gasStationAdapter.setGasStationList(gasStationInfos);
        mBinding.scenePoiDetailsGasStationView.poiGasBusinessHours.
                setText(getContext().getString(R.string.route_business_hour, info.getBusinessTime()));
        if (ConvertUtils.isEmpty(info.getPhone())) {
            mBinding.scenePoiDetailsGasStationView.poiGasPhone.setVisibility(View.GONE);
        }
        mBinding.scenePoiDetailsGasStationView.poiGasPhone.setText(
                getContext().getString(R.string.route_poi_phone, info.getPhone()));
        mBinding.scenePoiDetailsGasStationView.poiGasOilList.setLayoutManager(
                new GridLayoutManager(getContext(), mSpanCount));
        mBinding.scenePoiDetailsGasStationView.poiGasOilList.setAdapter(gasStationAdapter);
    }
}