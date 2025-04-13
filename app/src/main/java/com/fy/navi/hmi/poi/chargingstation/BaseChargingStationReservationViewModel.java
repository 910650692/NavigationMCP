package com.fy.navi.hmi.poi.chargingstation;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

public class BaseChargingStationReservationViewModel extends BaseViewModel<ChargingStationReservationFragment,ChargingStationReservationModel> {
    public BaseChargingStationReservationViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected ChargingStationReservationModel initModel() {
        return new ChargingStationReservationModel();
    }

    private final Action mRootClick = () -> {

    };

    public Action getRootClick() {
        return mRootClick;
    }
}
