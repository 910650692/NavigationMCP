package com.fy.navi.hmi.launcher;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.ui.base.BaseViewModel;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/27
 * Description: [在这里描述文件功能]
 */
public class BaseSmallCardPlaceHolderViewModel extends BaseViewModel<SmallCardPlaceHolderFragment, SmallCardPlaceHolderModel> {
    public BaseSmallCardPlaceHolderViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected SmallCardPlaceHolderModel initModel() {
        return new SmallCardPlaceHolderModel();
    }
}
