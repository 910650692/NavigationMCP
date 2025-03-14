package com.fy.navi.hmi.mapdata.manager;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

/**
 * @Description
 * @Author fh
 * @date 2025/03/13
 */
public class ManagerMapDataViewModel extends BaseViewModel<ManagerMapDataFragment, ManagerMapDataModel> {

    public ManagerMapDataViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected ManagerMapDataModel initModel() {
        return new ManagerMapDataModel();
    }

    /**
     * 返回上一页
     */
    public Action backMapDataView = () -> {
        closeFragment(true);
    };

    public Action downloadingDataClickView = () -> {

    };

    public Action downloadedClickView = () -> {

    };

    public Action allDataSuspend = () -> {

    };

    public Action allDataDownload = () -> {

    };

}
