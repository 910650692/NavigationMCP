package com.fy.navi.hmi.traffic;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.base.BaseViewModel;

/**
 * Author: QiuYaWei
 * Date: 2025/3/1
 * Description: [在这里描述文件功能]
 */
public class BaseBigPicDetailViewModel extends BaseViewModel<BigPicDetailFragment, BaseBigPicDetailModel> {
    public BaseBigPicDetailViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected BaseBigPicDetailModel initModel() {
        return new BaseBigPicDetailModel();
    }
}
