package com.fy.navi.hmi.route;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.constraintlayout.widget.ConstraintSet;
import androidx.recyclerview.widget.ItemTouchHelper;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentRouteBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.dialog.MsgTopDialog;
import com.fy.navi.scene.ui.adapter.RoutePOIGasStationAdapter;
import com.fy.navi.scene.ui.adapter.RoutePOIIconAdapter;
import com.fy.navi.scene.ui.adapter.RouteViaPointAdapter;
import com.fy.navi.scene.ui.search.RouteRequestLoadingDialog;
import com.fy.navi.scene.ui.search.RouteSearchLoadingDialog;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ServiceAreaInfo;
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
    private RouteRequestLoadingDialog mRouteRequestLoadingDialog;
    private RouteSearchLoadingDialog mSearchLoadingDialog;
    private RouteViaPointAdapter mRouteViaPointAdapter;
    private RoutePOIIconAdapter mPoiIconAdapter;
    private RoutePOIGasStationAdapter mGasStationAdapter;
    private MsgTopDialog mMsgTopDialog;
    private Map<PoiInfoEntity, View> mRouteChargeProgressViews;

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
        initDialog();
        initRouteScene();
        initTouchCloseTimer();
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
    }
    /***
     * 算路请求加载框
     */
    private void initDialog() {
        mRouteRequestLoadingDialog = new RouteRequestLoadingDialog(
                StackManager.getInstance().getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()));
        mRouteRequestLoadingDialog.setOnCloseClickListener(mViewModel);
        mSearchLoadingDialog = new RouteSearchLoadingDialog(
                StackManager.getInstance().getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()));
    }
    /***
     * Scene 初始化
     */
    private void initRouteScene() {
        mBinding.routeLineInfoSceneRouteResult.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeDetailInfoSceneRouteDetailsResult.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeLineInfoSceneRoutePerference.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeRightTabListScene.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeRightTabListChargeScene.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeDetailInfoSceneRouteSearchRefresh.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeChargeInfoSceneRouteSearchRefresh.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeLineInfoSceneRoutePerference.registerRoutePreferenceObserver(TAG, mViewModel);
        mBinding.routeLineInfoSceneRouteResult.registerRouteSelectObserver(TAG, mViewModel);
        mBinding.routeDetailInfoSceneRouteDetailsResult.registerRouteDeatailsCheckedObserver(TAG, mViewModel);
        mBinding.routeRightTabListScene.registerRouteSelectObserver(TAG, mViewModel);
        mBinding.routeRightTabListChargeScene.registerRouteSelectObserver(TAG, mViewModel);
        mBinding.routeDetailInfoSceneRouteSearchRefresh.registerRouteSearchRefreshObserver(TAG, mViewModel);
        mBinding.routeChargeInfoSceneRouteSearchRefresh.registerRouteSearchRefreshObserver(TAG, mViewModel);
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

    @Override
    public void onInitData() {
        mViewModel.setDefultPlateNumberAndAvoidLimitSave();
        final Bundle bundle = getArguments();
        assert bundle != null;
        final RouteSpeechRequestParam param = (RouteSpeechRequestParam) bundle.getSerializable("speech_open_route");
        if (!ConvertUtils.isEmpty(param)){
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
        if (!ConvertUtils.isEmpty(poiInfoEntity)){
            Logger.i(TAG, "nomal: " + ConvertUtils.isEmpty(poiInfoEntity));
            mViewModel.getTitle().set(poiInfoEntity.getName());
            mViewModel.getEndName().set(poiInfoEntity.getName());
            mBinding.routeDetailInfoSceneRouteDetailsResult.setEndPoint(poiInfoEntity.getName());
            final RouteRequestParam routeRequestParam = new RouteRequestParam();
            routeRequestParam.setMPoiInfoEntity(poiInfoEntity);
            routeRequestParam.setMRoutePoiType(poiType);
            mViewModel.requestRoute(routeRequestParam);
            return;
        }
        closeFragment(true);
        ToastUtils.Companion.getInstance().showCustomToastView("数据异常，无法进行路线规划");
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            mViewModel.isRequestRouteForPlateNumberAndAvoidLimitChange();
            mBinding.routeLineInfoSceneRoutePerference.resetPreference();
        }
    }
    /***
     * 设置规避UI
     * @param isAvoid true
     */
    public void setAvoidStatusUI(final boolean isAvoid) {
        mBinding.routeDetailInfoSceneRouteDetailsResult.setAvoidStatus(isAvoid);
    }
    /***
     * 点击规避
     */
    public void startAvoidRoad() {
        mViewModel.getRouteDetailsVisibility().set(true);
        setAvoidStatusUI(false);
        mBinding.routeDetailInfoSceneRouteDetailsResult.startAvoidRoad();
    }
    /***
     * 渲染算路结果列表
     * @param routeLineInfos 数据
     */
    public void setRouteResultListUI(final List<RouteLineInfo> routeLineInfos) {
        mBinding.routeLineInfoSceneRouteResult.notifyResultList(routeLineInfos);
    }
    /***
     * 渲染导航段
     * @param routeLineSegmentInfos 数据
     */
    public void setDetailsResult(final List<RouteLineSegmentInfo> routeLineSegmentInfos) {
        mBinding.routeDetailInfoSceneRouteDetailsResult.notifyRouteDetailsResultList(routeLineSegmentInfos);
    }
    /***
     * 设置补能规划开关
     */
    public void setEnergyChecked() {
        mBinding.routeLineInfoSwitchEnergy.setChecked(!mBinding.routeLineInfoSwitchEnergy.isChecked());
    }
    /***
     * 获取补能规划开关
     *
     * @return 开关状态
     */
    public boolean getEnergyChecked() {
        return mBinding.routeLineInfoSwitchEnergy.isChecked();
    }
    /***
     * 算路请求弹框展示
     */
    public void showProgressUI() {
        mRouteRequestLoadingDialog.show();
    }
    /***
     * 算路请求弹框关闭
     */
    public void hideProgressUI() {
        mRouteRequestLoadingDialog.hide();
    }
    /***
     * 搜索请求弹框开启
     */
    public void showSearchProgressUI() {
        mSearchLoadingDialog.show();
    }
    /***
     * 搜索请求弹框关闭
     */
    public void hideSearchProgressUI() {
        mSearchLoadingDialog.hide();
    }
    /***
     * 开启Trip弹框
     * @param title
     * @param content
     */
    public void showTripDialog(final String title, final String content) {
        if (mBinding.routeLineInfoSwitchEnergy.isChecked()){
            return;
        }
        if (!ConvertUtils.isEmpty(mMsgTopDialog) && mMsgTopDialog.isShowing()){
            return;
        }
        mViewModel.cancelTimer();
        mMsgTopDialog = new MsgTopDialog(
                StackManager.getInstance().getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()), TripID.ROUTE_LOW_BATTER);
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
        mRouteViaPointAdapter.setRouteBeanList(routeParams);
    }
    /***
     * 去除Tab选中
     */
    public void clearSceneTabUI() {
        mBinding.routeRightTabListScene.clearSceneTabUI();
    }
    /***
     * 去除Tab选中
     * @param isCharging 是否是充电
     */
    public void clearSceneTabUI(final boolean isCharging) {
        mBinding.routeRightTabListTvGasStation.setSelected(isCharging);
        mBinding.routeRightTabListIvGasStation.setSelected(isCharging);
    }
    /***
     * 展示服务区列表
     * @param poiInfoEntities 数据
     */
    public void showRouteSearchListUI(final List<RouteRestAreaDetailsInfo> poiInfoEntities) {
        mBinding.routeDetailInfoSceneRouteSearchRefresh.notifyResultList(poiInfoEntities);
    }
    /***
     * 展示充电站列表
     * @param poiInfoEntities 搜索数据
     * @param gasChargeAlongList 本地已添加数据
     * @param searchType 搜索方式 0-沿途搜索
     */
    public void showRouteSearchChargeListUI(final List<PoiInfoEntity> poiInfoEntities
            , final List<RouteParam> gasChargeAlongList, final int searchType) {
        mBinding.routeChargeInfoSceneRouteSearchRefresh.notifyResultList(poiInfoEntities, gasChargeAlongList ,searchType);
    }
    /***
     * 高亮沿途按钮
     */
    public void highlightAlongTab() {
        mBinding.routeRightTabListChargeScene.highlightAlongTab();
    }
    /***
     * 更新充电本地数据
     * @param gasChargeAlongList 本地数据
     * @param listSearchType 搜索方式
     */
    public void updateChareList(final List<RouteParam> gasChargeAlongList, final int listSearchType) {
        mBinding.routeChargeInfoSceneRouteSearchRefresh.updateChargeList(gasChargeAlongList ,listSearchType);
    }
    /***
     * 更新选中路线
     * @param routeIndex 选中路线索引
     */
    public void updateSelectRouteUI(final int routeIndex) {
        mBinding.routeLineInfoSceneRouteResult.updateSelectRouteUI(routeIndex);
    }
    /***
     * 更新路径
     * @param progress 百分比
     */
    public void updateRouteChargeExhaustUi(final float progress) {
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
            constraintSet.connect(customViewItem.getId(), ConstraintSet.END,  routeChargeProgressLayout.getId(), ConstraintSet.END);
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
                    if (view != null) {
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
        if (ConvertUtils.isEmpty(info.getPhone())){
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setVisibility(View.GONE);
            mBinding.stlPhone.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setVisibility(View.VISIBLE);
            mBinding.stlPhone.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + info.getPhone());
        }

        if (ConvertUtils.isEmpty(info.getBusinessTime())){
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_bussTime) + info.getBusinessTime());
        }

        if (!ConvertUtils.isEmpty(info.getServiceAreaInfoList())
                && !info.getServiceAreaInfoList().isEmpty() && !ConvertUtils.isEmpty(info.getServiceAreaInfoList().get(0))) {
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
                            &&  isNeed(child.getTypeCode())) {
                        codes.add(child.getTypeCode());
                        serviceAreaChildArrayList.add(child);
                    }
                }
                mPoiIconAdapter.setRouteBeanList(serviceAreaChildArrayList);
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
                    mGasStationAdapter.setRouteBeanList(gasString);
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
        ViewAdapterKt.loadImageUrl(mBinding.scenePoiDetailsChargingStationView.poiChargeImg
                , info.getImageUrl(), com.fy.navi.scene.R.drawable.test_pic, com.fy.navi.scene.R.drawable.test_pic);
        if (ConvertUtils.isEmpty(info.getPhone())){
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.GONE);
            mBinding.stlPhone.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.VISIBLE);
            mBinding.stlPhone.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + info.getPhone());
        }

        if (ConvertUtils.isEmpty(info.getBusinessTime())){
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
                    .setText(chargeInfo.getFastPower() + "kw." + chargeInfo.getFastVolt() +"v");

            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(chargeInfo.getSlow_free() + "");
            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getSlow_total());
            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot
                    .setText(chargeInfo.getSlowPower() + "kw." + chargeInfo.getSlowVolt() +"v");

            mBinding.scenePoiDetailsChargingStationView.poiChargePrice
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free)
                    + chargeInfo.getCurrentElePrice() + ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free_unit));
            mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice
                    .setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_park_free)
                    + chargeInfo.getCurrentServicePrice());
        }
    }
}