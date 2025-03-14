package com.fy.navi.service.adapter.search.bls;

import androidx.annotation.Nullable;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class SearchCallbackWrapper<T> implements IBLSearchCallback<T> {
    @Nullable
    private final IBLSearchCallback<T> bllSearchCallback;

    public SearchCallbackWrapper(@Nullable IBLSearchCallback<T> oriCallback) {
        this.bllSearchCallback = oriCallback;
    }

    @Override
    public void onSuccess(T data) {
        if (bllSearchCallback == null) {
            return;
        }
        bllSearchCallback.onSuccess(data);
    }

    @Override
    public void onFailure(int errCode, T data) {
        if (bllSearchCallback == null) {
            return;
        }
        bllSearchCallback.onFailure(errCode, data);
    }

    @Override
    public void onComplete() {
        if (bllSearchCallback == null) {
            return;
        }
        bllSearchCallback.onComplete();
    }
}

