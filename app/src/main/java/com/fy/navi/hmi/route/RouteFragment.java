package com.fy.navi.hmi.route;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.recyclerview.widget.ItemTouchHelper;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
@Route(path = RoutePath.Route.ROUTE_FRAGMENT)
public class RouteFragment extends BaseFragment<FragmentRouteBinding, RouteViewModel> {
    private static final String TAG = RouteFragment.class.getSimpleName();
    private RouteRequestLoadingDialog routeRequestLoadingDialog;
    private RouteSearchLoadingDialog searchLoadingDialog;
    private RouteViaPointAdapter routeViaPointAdapter;
    private RoutePOIIconAdapter poiIconAdapter;
    private RoutePOIGasStationAdapter gasStationAdapter;
    private MsgTopDialog msgTopDialog;

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
        initElecCheckBox();
        initViaPoiAdaper();
        initDetailsAdaper();
        initDialog();
        initRouteScene();
        initTouchCloseTimer();
    }
    private void initElecCheckBox() {
        mBinding.routeLineInfoSwitchEnergy.setChecked(SettingPackage.getInstance().getChargingPlan());
        mBinding.routeLineInfoSwitchEnergy.setOnCheckedChangeListener((compoundButton, b) -> {
            mViewModel.cancelTimer();
            if (b) hideTrip();
            BevPowerCarUtils.getInstance().isElecPlanRoute = b;
            SettingPackage.getInstance().setChargingPlan(b);
            mViewModel.requestRoute(null, -1, RouteWayID.ROUTE_WAY_REFRESH);
        });
        BevPowerCarUtils.getInstance().isElecPlanRoute = mBinding.routeLineInfoSwitchEnergy.isChecked();
    }

    private void initViaPoiAdaper() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.routeLineViaPoiRecycle.setLayoutManager(layoutManager);
        routeViaPointAdapter = new RouteViaPointAdapter();
        routeViaPointAdapter.setDeleteViaPointListener(index -> {
            mViewModel.cancelTimer();
            mViewModel.deleteViaParamMode(index);
        });
        mBinding.routeLineViaPoiRecycle.setAdapter(routeViaPointAdapter);
        ItemTouchHelper touchHelper = new ItemTouchHelper(new ItemTouchHelper.Callback() {
            private int movePosition = -1;
            private int currentPosition = -1;
            @Override
            public int getMovementFlags(@NonNull RecyclerView recyclerView, @NonNull RecyclerView.ViewHolder viewHolder) {
                int dragFlags = ItemTouchHelper.UP | ItemTouchHelper.DOWN;
                int swipeFlags = ItemTouchHelper.START | ItemTouchHelper.END;
                return makeMovementFlags(dragFlags, swipeFlags);
            }
            @Override
            public boolean onMove(@NonNull RecyclerView recyclerView, @NonNull RecyclerView.ViewHolder viewHolder, @NonNull RecyclerView.ViewHolder target) {
                currentPosition = viewHolder.getAdapterPosition();
                movePosition = target.getAdapterPosition();
                routeViaPointAdapter.onItemMove(viewHolder.getAdapterPosition(), target.getAdapterPosition());
                return true;
            }
            @Override
            public void onSwiped(@NonNull RecyclerView.ViewHolder viewHolder, int direction) {
            }
            @Override
            public void clearView(@NonNull RecyclerView recyclerView, @NonNull RecyclerView.ViewHolder viewHolder) {
                super.clearView(recyclerView, viewHolder);
                mViewModel.changeParamListMode(currentPosition, movePosition);
            }
            @Override
            public boolean isItemViewSwipeEnabled() {
                return false;
            }
        });
        touchHelper.attachToRecyclerView(mBinding.routeLineViaPoiRecycle);
    }
    private void initDetailsAdaper() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.HORIZONTAL);
        mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setLayoutManager(layoutManager);
        poiIconAdapter = new RoutePOIIconAdapter();
        mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setAdapter(poiIconAdapter);

        LinearLayoutManager layoutManager1 = new LinearLayoutManager(getContext());
        layoutManager1.setOrientation(LinearLayoutManager.HORIZONTAL);
        mBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.setLayoutManager(layoutManager1);
        gasStationAdapter = new RoutePOIGasStationAdapter();
        mBinding.scenePoiDetailsServiceAreaView.routePoidetailGasStation.setAdapter(gasStationAdapter);
    }
    private void initDialog() {
        routeRequestLoadingDialog = new RouteRequestLoadingDialog(StackManager.getInstance().getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()));
        searchLoadingDialog = new RouteSearchLoadingDialog(StackManager.getInstance().getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()));
    }
    private void initRouteScene() {
        mBinding.routeLineInfoSceneRouteResult.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeDetailInfoSceneRouteDetailsResult.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeLineInfoSceneRoutePerference.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeRightTabListScene.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeRightTabListChargeScene.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeDetailInfoSceneRouteSearchRefresh.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeChargeInfoSceneRouteSearchRefresh.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.routeLineInfoSceneRoutePerference.registerRoutePreferenceObserver("route fragment", mViewModel);
        mBinding.routeLineInfoSceneRouteResult.registerRouteSelectObserver("route fragment", mViewModel);
        mBinding.routeDetailInfoSceneRouteDetailsResult.registerRouteDeatailsCheckedObserver("route fragment", mViewModel);
        mBinding.routeRightTabListScene.registerRouteSelectObserver("route fragment", mViewModel);
        mBinding.routeRightTabListChargeScene.registerRouteSelectObserver("route fragment", mViewModel);
        mBinding.routeDetailInfoSceneRouteSearchRefresh.registerRouteSearchRefreshObserver("route fragment", mViewModel);
        mBinding.routeChargeInfoSceneRouteSearchRefresh.registerRouteSearchRefreshObserver("route fragment", mViewModel);
    }
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
        Bundle bundle = getArguments();
        assert bundle != null;
        RouteSpeechRequestParam param = (RouteSpeechRequestParam) bundle.getSerializable("speech_open_route");
        Logger.i(TAG, "speech: " + ConvertUtils.isEmpty(param));
        if (!ConvertUtils.isEmpty(param)){
            mViewModel.requestRouteFromSpeech(param);
            return;
        }

        RouteMsgPushInfo routeMsgPushInfo = (RouteMsgPushInfo) bundle.getSerializable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MSG_PUSH_OPEN_ROUTE_TYPE);
        if (routeMsgPushInfo != null) {
            Logger.i(TAG, "nomal: " + GsonUtils.toJson(routeMsgPushInfo));
            mViewModel.title.set(routeMsgPushInfo.getName());
            mViewModel.endName.set(routeMsgPushInfo.getName());
            mBinding.routeDetailInfoSceneRouteDetailsResult.setEndPoint(routeMsgPushInfo.getName());
            mViewModel.requestRouteRestoration(routeMsgPushInfo);
            return;
        }


        PoiInfoEntity poiInfoEntity = (PoiInfoEntity) bundle.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE);
        int poiType = (int) bundle.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE);
        Logger.i(TAG, "nomal: " + ConvertUtils.isEmpty(poiInfoEntity));
        if (ConvertUtils.isEmpty(poiInfoEntity)) return;
        mViewModel.title.set(poiInfoEntity.getName());
        mViewModel.endName.set(poiInfoEntity.getName());
        mBinding.routeDetailInfoSceneRouteDetailsResult.setEndPoint(poiInfoEntity.getName());
        mViewModel.requestRoute(poiInfoEntity, poiType, RouteWayID.ROUTE_WAY_DEFAULT);
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            mViewModel.isRequestRouteForPlateNumberAndAvoidLimitChange();
            mBinding.routeLineInfoSceneRoutePerference.resetPreference();
        }
    }
    public void setAvoidStatusUI(boolean isAvoid) {
        mBinding.routeDetailInfoSceneRouteDetailsResult.setAvoidStatus(isAvoid);
    }

    public void startAvoidRoad() {
        mViewModel.routeDetailsVisibility.set(true);
        setAvoidStatusUI(false);
        mBinding.routeDetailInfoSceneRouteDetailsResult.startAvoidRoad();
    }
    public void setRouteResultListUI(List<RouteLineInfo> routeLineInfos) {
        mBinding.routeLineInfoSceneRouteResult.notifyResultList(routeLineInfos);
    }
    public void setDetailsResult(List<RouteLineSegmentInfo> routeLineSegmentInfos) {
        mBinding.routeDetailInfoSceneRouteDetailsResult.notifyRouteDetailsResultList(routeLineSegmentInfos);
    }
    public void setEnergyChecked() {
        mBinding.routeLineInfoSwitchEnergy.setChecked(!mBinding.routeLineInfoSwitchEnergy.isChecked());
    }
    public boolean getEnergyChecked () {
        return mBinding.routeLineInfoSwitchEnergy.isChecked();
    }
    public void showProgressUI() {
        routeRequestLoadingDialog.show();
    }
    public void hideProgressUI() {
        routeRequestLoadingDialog.hide();
    }
    public void showSearchProgressUI() {
        searchLoadingDialog.show();
    }
    public void hideSearchProgressUI() {
        searchLoadingDialog.hide();
    }
    public void showTripDialog(String title, String content) {
        if (mBinding.routeLineInfoSwitchEnergy.isChecked()) return;
        if (!ConvertUtils.isEmpty(msgTopDialog) && msgTopDialog.isShowing()) return;
        mViewModel.cancelTimer();
        msgTopDialog = new MsgTopDialog(StackManager.getInstance().getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()), TripID.ROUTE_LOW_BATTER);
        msgTopDialog.setTitle(title);
        msgTopDialog.setContent(content);
        msgTopDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick(TripID tripID) {
                if (tripID == TripID.ROUTE_LOW_BATTER) {
                    mBinding.routeLineInfoSwitchEnergy.setChecked(true);
                }
            }
        });
        msgTopDialog.showDialog();
    }
    public void hideTrip() {
        if (!ConvertUtils.isEmpty(msgTopDialog)) {
            msgTopDialog.cancel();
            msgTopDialog = null;
        }
    }
    public void setViaList(List<RouteParam> routeParams) {
        routeViaPointAdapter.setRouteBeanList(routeParams);
    }
    public void clearSceneTabUI() {
        mBinding.routeRightTabListScene.clearSceneTabUI();
    }

    public void clearSceneTabUI(boolean isCharging) {
        mBinding.routeRightTabListTvGasStation.setSelected(isCharging);
        mBinding.routeRightTabListIvGasStation.setSelected(isCharging);
    }
    public void showRouteSearchListUI(List<PoiInfoEntity> poiInfoEntities) {
        mBinding.routeDetailInfoSceneRouteSearchRefresh.notifyResultList(poiInfoEntities);
    }

    public void showRouteSearchChargeListUI(List<PoiInfoEntity> poiInfoEntities, List<RouteParam> gasChargeAlongList, int searchType) {
        mBinding.routeChargeInfoSceneRouteSearchRefresh.notifyResultList(poiInfoEntities, gasChargeAlongList ,searchType);
    }
    public void highlightAlongTab() {
        mBinding.routeRightTabListChargeScene.highlightAlongTab();
    }
    public void updateChareList(List<RouteParam> gasChargeAlongList, int listSearchType) {
        mBinding.routeChargeInfoSceneRouteSearchRefresh.updateChargeList(gasChargeAlongList ,listSearchType);
    }
    public void updateSelectRouteUI(int routeIndex) {
        mBinding.routeLineInfoSceneRouteResult.updateSelectRouteUI(routeIndex);
    }
    public RouteLineInfo getSelectLineInfo() {
        return mBinding.routeLineInfoSceneRouteResult.getSelectLineInfo();
    }
    public int getCurrentRouteIndex() {
        return mBinding.routeLineInfoSceneRouteResult.getCurrentIndex();
    }
    public void updateRouteChargeExhaustUi(float progress) {
        ThreadManager.getInstance().postUi(() -> {
            ConstraintLayout.LayoutParams layoutParams = (ConstraintLayout.LayoutParams) mBinding.routeChargeExhaustionPoint.getLayoutParams();
            layoutParams.horizontalBias = progress;
            mBinding.routeChargeExhaustionPoint.setLayoutParams(layoutParams);
        });
    }
    @SuppressLint("SetTextI18n")
    public void showServiceDetailsUI(PoiInfoEntity info) {
        if (ConvertUtils.isEmpty(info.getPhone())){
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setVisibility(View.GONE);
            mBinding.stlPhone.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setVisibility(View.VISIBLE);
            mBinding.stlPhone.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaPhone.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + info.getPhone());
        }

        if (ConvertUtils.isEmpty(info.getBusinessTime())){
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaHours.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_bussTime) + info.getBusinessTime());
        }

        if (!ConvertUtils.isEmpty(info.getServiceAreaInfoList()) && !info.getServiceAreaInfoList().isEmpty() && !ConvertUtils.isEmpty(info.getServiceAreaInfoList().get(0))) {
            List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildList = info.getServiceAreaInfoList().get(0).getServiceAreaChildList();
            int building = info.getServiceAreaInfoList().get(0).getBuilding();
            mViewModel.routeSearchStatusVisibility.set(true);
            switch (building) {
                case 0:
                    mViewModel.routeSearchStatusVisibility.set(false);
                    break;
                case 1:
                    mViewModel.routeSearchStatus.set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_building));
                    break;
                case 2:
                    mViewModel.routeSearchStatus.set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_not_find));
                    break;
                case 3:
                    mViewModel.routeSearchStatus.set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_starting));
                    break;
                case 4:
                    mViewModel.routeSearchStatus.set(ResourceUtils.Companion.getInstance().getString(R.string.route_details_stoped));
                    break;
            }
            if (!serviceAreaChildList.isEmpty()) {
                mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setVisibility(View.VISIBLE);
                List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildArrayList = new ArrayList<>();
                List<String> codes = new ArrayList<>();
                for (ServiceAreaInfo.ServiceAreaChild child : serviceAreaChildList) {
                    if (!codes.contains(child.getTypeCode())
                            &&  isNeed(child.getTypeCode())) {
                        codes.add(child.getTypeCode());
                        serviceAreaChildArrayList.add(child);
                    }
                }
                poiIconAdapter.setRouteBeanList(serviceAreaChildArrayList);
                String gasType = "";
                for (ServiceAreaInfo.ServiceAreaChild child : serviceAreaChildArrayList) {
                    if (!ConvertUtils.isEmpty(child.getGasType())) {
                        gasType = child.getGasType();
                    }
                }
                if (ConvertUtils.isEmpty(gasType)) {
                    mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(View.GONE);
                } else {
                    List<String> gasString = new ArrayList<>();
                    if (gasType.contains("|")) {
                        String[] split = gasType.split("\\|");
                        gasString.addAll(Arrays.asList(split));
                    } else {
                        gasString.add(gasType);
                    }
                    gasStationAdapter.setRouteBeanList(gasString);
                    mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(View.VISIBLE);
                }
            } else {
                mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaFacility.setVisibility(View.GONE);
                mBinding.scenePoiDetailsServiceAreaView.poiServiceAreaOil.setVisibility(View.GONE);
            }
        }
    }
    private boolean isNeed(String typeCode) {
        return typeCode.equals("010200") || typeCode.equals("200300")
                || typeCode.equals("060400") || typeCode.equals("150904")
                || typeCode.equals("011100") || typeCode.equals("010000")
                || typeCode.equals("050100");
    }
    @SuppressLint("SetTextI18n")
    public void showChargeDetailsUI(PoiInfoEntity info) {
        ViewAdapterKt.loadImageUrl(mBinding.scenePoiDetailsChargingStationView.poiChargeImg, info.getImageUrl(), com.fy.navi.scene.R.drawable.test_pic, com.fy.navi.scene.R.drawable.test_pic);
        if (ConvertUtils.isEmpty(info.getPhone())){
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.GONE);
            mBinding.stlPhone.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setVisibility(View.VISIBLE);
            mBinding.stlPhone.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsChargingStationView.poiChargeAreaPhone.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_phone) + info.getPhone());
        }

        if (ConvertUtils.isEmpty(info.getBusinessTime())){
            mBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setVisibility(View.GONE);
        } else {
            mBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setVisibility(View.VISIBLE);
            mBinding.scenePoiDetailsChargingStationView.poiCharegBusinessHours.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_poi_details_bussTime) + info.getBusinessTime());
        }
        if (!ConvertUtils.isEmpty(info.getChargeInfoList()) && !info.getChargeInfoList().isEmpty()) {
            ChargeInfo chargeInfo = info.getChargeInfoList().get(0);
            mBinding.scenePoiDetailsChargingStationView.poiChargeFastOccupied.setText(chargeInfo.getFast_free() + "");
            mBinding.scenePoiDetailsChargingStationView.poiChargeFastTotal.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getFast_free());
            mBinding.scenePoiDetailsChargingStationView.poiChargeFastCurrentAndVlot.setText(chargeInfo.getFastPower() + "kw." + chargeInfo.getFastVolt() +"v");

            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowOccupied.setText(chargeInfo.getSlow_free() + "");
            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowTotal.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_jg) + chargeInfo.getSlow_total());
            mBinding.scenePoiDetailsChargingStationView.poiChargeSlowCurrentAndVlot.setText(chargeInfo.getSlowPower() + "kw." + chargeInfo.getSlowVolt() +"v");

            mBinding.scenePoiDetailsChargingStationView.poiChargePrice.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free) +chargeInfo.getCurrentElePrice() + ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_free_unit));
            mBinding.scenePoiDetailsChargingStationView.poiChargeParkPrice.setText(ResourceUtils.Companion.getInstance().getString(R.string.route_details_charge_park_free) +chargeInfo.getCurrentServicePrice());
        }
    }
}