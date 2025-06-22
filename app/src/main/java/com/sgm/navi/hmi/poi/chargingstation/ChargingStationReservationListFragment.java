package com.sgm.navi.hmi.poi.chargingstation;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentChargingStationReservationListBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.EquipmentInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.ReservationInfo;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;

@Route(path = RoutePath.Search.POI_CHARGE_RESERVATION_LIST_FRAGMENT)
public class ChargingStationReservationListFragment extends BaseFragment<FragmentChargingStationReservationListBinding, BaseChargingStationReservationListViewModel> {

    @Override
    public void onCreateBefore() {
        super.onCreateBefore();
    }

    @Override
    public int onLayoutId() {
        return R.layout.fragment_charging_station_reservation_list;
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
        mBinding.sceneChargeReservationList.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

    @Override
    public void onInitData() {
        getPriceListData();
    }

    private void getPriceListData(){
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        final int type = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_CHARGE_EQUIPMENT_TYPE);
        final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_CHARGE_PRICE_LIST);
        mBinding.sceneChargeReservationList.notifyEquipmentInfo(type,poiInfoEntity);
    }

    public void notifyCreateReservationSuccess(int taskId,String preNum){
        mBinding.sceneChargeReservationList.notifyCreateReservationSuccess(taskId,preNum);
    }

    public void onSearchError(int taskId,String message){
        mBinding.sceneChargeReservationList.onSearchError(taskId,message);
    }

    public void notifyEquipmentResult(int taskId,EquipmentInfo info){
        mBinding.sceneChargeReservationList.notifyEquipmentResult(taskId,info);
    }

    public void notifyUnLockResult(int taskId){
        mBinding.sceneChargeReservationList.notifyUnLockResult(taskId);
    }

    public void notifyCancelReservation(int taskId,ArrayList<ReservationInfo> list){
        mBinding.sceneChargeReservationList.notifyCancelReservation(taskId,list);
    }

    public void notifyCancelSuccess(int taskId){
        mBinding.sceneChargeReservationList.notifyCancelSuccess(taskId);
    }

    public void notifyReadyReservation(int taskId,ArrayList<ReservationInfo> list){
        mBinding.sceneChargeReservationList.notifyReadyReservation(taskId,list);
    }
}