package com.fy.navi.scene.ui.poi;

import android.app.Activity;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SceneReservationDetailViewBinding;
import com.fy.navi.scene.impl.poi.ScenePoiChargingStationReservationViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.ConnectorInfoItem;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ReservationInfo;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import org.json.JSONException;
import org.json.JSONObject;

import java.text.MessageFormat;
import java.util.ArrayList;

public class ScenePoiChargingStationReservationView extends BaseSceneView<SceneReservationDetailViewBinding, ScenePoiChargingStationReservationViewImpl> {
    private PoiInfoEntity mPoiInfoEntity;
    private ReservationInfo mReservationInfo;
    private EquipmentInfo mEquipmentInfo;
    private Integer mCancelNumber = 0;
    private static final Integer MAX_CANCEL = 3;

    public ScenePoiChargingStationReservationView(@NonNull Context context) {
        super(context);
    }

    public ScenePoiChargingStationReservationView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiChargingStationReservationView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneReservationDetailViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneReservationDetailViewBinding.inflate(inflater,viewGroup,true);
    }

    @Override
    protected ScenePoiChargingStationReservationViewImpl initSceneImpl() {
        return new ScenePoiChargingStationReservationViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScenePoiChargingStationReservationView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    public void notifyPoiInfo(PoiInfoEntity poiInfo){
        mPoiInfoEntity = poiInfo;
        if(!ConvertUtils.isNull(mPoiInfoEntity.getReservationInfo())){
            mReservationInfo = mPoiInfoEntity.getReservationInfo();
            mScreenViewModel.queryEquipmentInfo(mReservationInfo);
            mScreenViewModel.queryReservation(mPoiInfoEntity,(Activity) getContext());
        }
        refreshEtaInfoView();
    }

    public void notifyEquipmentInfo(EquipmentInfo equipmentInfo){
        mEquipmentInfo = equipmentInfo;
        refreshEquipmentInfoView();
    }

    public void notifyLockGroundSuccess(){
        mScreenViewModel.queryEquipmentInfo(mReservationInfo);
    }

    public void notifyCancelSuccess(){
        mScreenViewModel.queryEquipmentInfo(mReservationInfo);
    }

    public void notifyCancelReservation(ArrayList<ReservationInfo> list){
        mCancelNumber = list.size();
    }

    public void closeFragment(){
        closeCurrentFragment();
    }

    private void refreshEtaInfoView() {
        mScreenViewModel.getTravelTimeFuture(new GeoPoint(mPoiInfoEntity.getPoint().getLon(),
                        mPoiInfoEntity.getPoint().getLat()))
                .thenAccept(etaInfo -> {
                    ThreadManager.getInstance().postUi(new Runnable() {
                        @Override
                        public void run() {
                            if (!ConvertUtils.isEmpty(etaInfo) && !ConvertUtils.isEmpty(mViewBinding)
                                    && !ConvertUtils.isEmpty(mViewBinding.poiDistanceTime)) {
                                final String distance = formatDistanceArrayInternal(
                                        etaInfo.getDistance());
                                mViewBinding.poiDistanceTime.setText(MessageFormat.format("{0} {1}",
                                        distance, etaInfo.getTravelTime()));
                                final int leftCharge = Math.max(-99, etaInfo.getLeftCharge());
                                if (!ConvertUtils.isEmpty(leftCharge)) {
                                    //50%以上电量，显示满电量图片，20-50%电量，显示半电量图片
                                    //0-20电量，显示低电量图片，文本变红
                                    //小于0%电量，显示空电量图片，文本变红
                                    if (leftCharge >= 50 && leftCharge <= 100) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_full_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.poi_details_bottom_ff_00));
                                    } else if (leftCharge > 20 && leftCharge < 50) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_medium_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.poi_details_bottom_ff_00));
                                    } else if (leftCharge > 0 && leftCharge <= 20) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_low_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_C73333_100));
                                    } else if (leftCharge <= 0) {
                                        mViewBinding.sivArrivalCapacity.setImageResource(R.drawable.img_electricity_empty_42);
                                        mViewBinding.poiArrivalCapacity.setTextColor(
                                                ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_C73333_100));
                                    }
                                }
                                mViewBinding.poiArrivalCapacity.setText(getContext().getString(
                                        R.string.remain_charge, leftCharge));
                            }
                        }
                    });

                })
                .exceptionally(error -> {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "getTravelTimeFuture error:" + error);
                    return null;
                });
    }

    private void refreshEquipmentInfoView(){
        ConnectorInfoItem connectorInfoItem = mEquipmentInfo.getmConnectorInfoItem().get(0);
        mViewBinding.chargeStationType.setText("0".equals(connectorInfoItem.getmChargeType()) ? R.string.charge_title_slow : R.string.charge_title_fast);
        mViewBinding.chargeStationStatus.setText(connectorInfoItem.getStatusName(connectorInfoItem.getmStatus()));
        if(ConvertUtils.isEmpty(connectorInfoItem.getmParkNo())){
            mViewBinding.chargeStationDeviceName.setText(getContext().getString(R.string.charge_equipment_number,connectorInfoItem.getmConnectorId()));
            mViewBinding.chargeStationDeviceNumber.setVisibility(GONE);
        }else{
            mViewBinding.chargeStationDeviceName.setText(getContext().getString(R.string.charge_equipment_pack_no,connectorInfoItem.getmParkNo()));
            mViewBinding.chargeStationDeviceNumber.setVisibility(VISIBLE);
            mViewBinding.chargeStationDeviceNumber.setText(getContext().getString(R.string.charge_equipment_number,connectorInfoItem.getmConnectorId()));
        }
        mViewBinding.poiChargeUnlock.setText(connectorInfoItem.getmLockStatus() == 10 ? R.string.charge_lock : R.string.charge_unlock);
        mViewBinding.skPoiName.setText(mPoiInfoEntity.getName());
        mViewBinding.poiSecondAddress.setText(mPoiInfoEntity.getAddress());
        mViewBinding.poiCancelReservation.setVisibility(connectorInfoItem.getmPreFlag() == 2 ? VISIBLE : GONE);
        mViewBinding.poiChargeUnlock.setOnClickListener(v -> {
            if(connectorInfoItem.getmLockStatus() == 50){
                return;
            }
            openUnLockDialog();
        });
        mViewBinding.poiGoHere.setOnClickListener(v -> {
            openRouteFragment(mPoiInfoEntity);
        });
        mViewBinding.poiCancelReservation.setOnClickListener(v -> {
            cancelReservation();
        });
    }

    private String formatDistanceArrayInternal(final int distance) {
        final String[] distanceArray = ConvertUtils.formatDistanceArray(AppCache.getInstance().getMContext(), distance);
        return distanceArray[0] + distanceArray[1];
    }

    private void openUnLockDialog(){
        new ChargeStationConfirmDialog.Build(getContext()).setDialogObserver(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                mScreenViewModel.unGroundLock(mEquipmentInfo.getmConnectorInfoItem().get(0),mPoiInfoEntity,(Activity) getContext());
            }
        })
        .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.sure_unlock))
        .setTip(ResourceUtils.Companion.getInstance().getString(R.string.sure_unlock_detail))
        .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.dsc_confirm))
        .build().show();
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_GO)
    public void openRouteFragment(PoiInfoEntity poiInfoEntity){
        final Fragment fragment = (Fragment) ARouter.getInstance().build(
                RoutePath.Route.ROUTE_FRAGMENT).navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));

        //for burying point
        final JSONObject params = new JSONObject();
        try {
            params.put(BuryConstant.Key.ROUTE_POI_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
            params.put(BuryConstant.Key.POI_INFO_ENTRY, poiInfoEntity);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        final BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, params.toString())
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    private void cancelReservation(){
        new ChargeStationConfirmDialog.Build(getContext()).setDialogObserver(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
            if(mCancelNumber >= MAX_CANCEL){
                return;
            }
            mScreenViewModel.cancelReservation(mReservationInfo,(Activity) getContext());
            }
        })
        .setTitle(getContext().getString(R.string.cancel_num_tip,mCancelNumber.toString()))
        .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.dsc_confirm))
        .build().show();

    }
}
