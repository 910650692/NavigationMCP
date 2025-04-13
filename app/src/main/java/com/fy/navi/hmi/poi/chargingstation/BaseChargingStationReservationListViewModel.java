package com.fy.navi.hmi.poi.chargingstation;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.base.BaseViewModel;

public class BaseChargingStationReservationListViewModel extends BaseViewModel<ChargingStationReservationListFragment,ChargingStationReservationListModel> {
    public BaseChargingStationReservationListViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected ChargingStationReservationListModel initModel() {
        return new ChargingStationReservationListModel();
    }
}
