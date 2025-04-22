package com.fy.navi.service.adapter.search.bls;

import androidx.annotation.Nullable;

/**
 * @author baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 * @version \$Revision1.0\$
 * @param <T> 回调数据泛型
 */
public class SearchCallbackWrapper<T> implements IBLSearchCallback<T> {
    @Nullable
    private final IBLSearchCallback<T> mBllSearchCallback;

    public SearchCallbackWrapper(@Nullable final IBLSearchCallback<T> oriCallback) {
        this.mBllSearchCallback = oriCallback;
    }

    @Override
    public void onSuccess(final int taskId, final T data) {
        if (mBllSearchCallback == null) {
            return;
        }
        mBllSearchCallback.onSuccess(taskId, data);
    }

    @Override
    public void onFailure(final int errCode, final T data) {
        if (mBllSearchCallback == null) {
            return;
        }
        mBllSearchCallback.onFailure(errCode, data);
    }

    @Override
    public void onComplete() {
        if (mBllSearchCallback == null) {
            return;
        }
        mBllSearchCallback.onComplete();
    }
}

