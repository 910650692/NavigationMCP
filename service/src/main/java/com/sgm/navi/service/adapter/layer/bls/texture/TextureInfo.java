package com.sgm.navi.service.adapter.layer.bls.texture;

import android.graphics.Bitmap;

import lombok.Getter;

public final class TextureInfo {
    @Getter
    private Bitmap bitmap;
    @Getter
    private int resId;

    public TextureInfo(Bitmap bitmap, int resId) {
        this.bitmap = bitmap;
        this.resId = resId;
    }
}
