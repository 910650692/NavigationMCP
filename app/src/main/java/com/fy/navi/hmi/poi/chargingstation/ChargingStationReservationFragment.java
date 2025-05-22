package com.fy.navi.hmi.poi.chargingstation;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentChargingStationReservationBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ReservationInfo;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;

@Route(path = RoutePath.Search.POI_CHARGE_RESERVATION_DETAILS_FRAGMENT)
public class ChargingStationReservationFragment extends BaseFragment<FragmentChargingStationReservationBinding, BaseChargingStationReservationViewModel> {

    @Override
    public void onCreateBefore() {
        super.onCreateBefore();
    }

    @Override
    public int onLayoutId() {
        return R.layout.fragment_charging_station_reservation;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public int onFragmentId() {
        return super.onFragmentId();
    }

    @Override
    public void onInitView() {
        mBinding.sceneChargeReservation.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

    @Override
    public void onInitData() {
        getListData();
    }

    private void getListData(){
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_CHARGE_PRICE_LIST);
        mBinding.sceneChargeReservation.notifyPoiInfo(poiInfoEntity);
    }

    public void notifyEquipmentInfo(EquipmentInfo equipmentInfo){
        mBinding.sceneChargeReservation.notifyEquipmentInfo(equipmentInfo);
    }

    public void notifyLockGround(){
        mBinding.sceneChargeReservation.notifyLockGroundSuccess();
    }

    public void notifyCancelSuccess(){
        mBinding.sceneChargeReservation.notifyCancelSuccess();
    }

    public void notifyCancelReservation(ArrayList<ReservationInfo> list){
        mBinding.sceneChargeReservation.notifyCancelReservation(list);
    }
}