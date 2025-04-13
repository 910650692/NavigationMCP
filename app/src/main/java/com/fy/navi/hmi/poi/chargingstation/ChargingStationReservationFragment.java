package com.fy.navi.hmi.poi.chargingstation;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentChargingStationReservationBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseFragment;

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

    }
}