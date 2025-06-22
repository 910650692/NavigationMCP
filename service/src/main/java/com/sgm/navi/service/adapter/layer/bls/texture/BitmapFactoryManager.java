package com.sgm.navi.service.adapter.layer.bls.texture;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.VectorDrawable;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;

import androidx.vectordrawable.graphics.drawable.VectorDrawableCompat;

import com.android.utils.log.Logger;
import com.autonavi.gbl.map.layer.LayerItem;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.bls.style.IUpdateBitmapViewProcessor;
import com.sgm.navi.service.define.layer.refix.LayerItemData;

import java.util.concurrent.atomic.AtomicReference;

/**
 * Bitmap创建类
 */
public final class BitmapFactoryManager {

    private BitmapFactoryManager() {
    }

    public static BitmapFactoryManager get() {
        return Holder._INSTANCE;
    }

    private static final class Holder {
        public static final BitmapFactoryManager _INSTANCE = new BitmapFactoryManager();
    }

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private static final String MARK_FROM_RES_TAG = "/";
    private static final String MARK_FROM_DRAWABLE_RES = "drawable";
    private static final String MARK_FROM_MIPMAP_RES = "mipmap";
    private static final String MARK_FROM_LAYOUT_RES = "layout";
    private static final int CREATE_BITMAP_RETRY_COUNT = 3;
    /**
     * 默认错误的资源id
     */
    private static final int DEF_ERROR_ID = 0;

    private static final AtomicReference<Canvas> DEF_CANVAS = new AtomicReference<>(new Canvas());

    /**
     * 是否无效资源
     *
     * @param markerId
     * @return
     */
    public boolean isValid(int markerId) {
        return markerId > DEF_ERROR_ID;
    }

    public synchronized <D extends LayerItemData> TextureInfo createBitmap(Context context, String markerRes, boolean isFocus, LayerItem item, IUpdateBitmapViewProcessor<D> processor, D data) {
        TextureInfo textureInfo = new TextureInfo(null, DEF_ERROR_ID);
        if (TextUtils.isEmpty(markerRes) || "-1".equals(markerRes) || markerRes.endsWith(".xml")) {
            Logger.v(TAG, "markerRes 资源 无效 : " + markerRes);
            return textureInfo;
        }
        String[] tag = markerRes.split(MARK_FROM_RES_TAG);
        int resID = DEF_ERROR_ID;
        if (tag.length == 2) {
            resID = context.getResources().getIdentifier(tag[1], tag[0], context.getPackageName());
        } else if (tag.length == 1) {
            resID = context.getResources().getIdentifier(tag[0], MARK_FROM_DRAWABLE_RES, context.getPackageName());
            if (!isValid(resID)) {
                resID = context.getResources().getIdentifier(tag[0], MARK_FROM_MIPMAP_RES, context.getPackageName());
                if (!isValid(resID)) {
                    resID = context.getResources().getIdentifier(tag[0], MARK_FROM_LAYOUT_RES, context.getPackageName());
                }
            }
        }
        if (!isValid(resID)) {
            Logger.v(TAG, "markerRes 资源无效，请检查是否存在: " + markerRes);
            return textureInfo;
        }
        Drawable drawable = null;
        View rootView = null;
        try {
            drawable = context.getResources().getDrawable(resID, null);
        } catch (Exception exception) {
            Logger.v(TAG, markerRes + " 不存在 Drawable / mipmap 文件夹中");
        } finally {
            try {
                rootView = LayoutInflater.from(context).inflate(resID, null, false);
            } catch (Exception exception) {
                Logger.v(TAG, markerRes + " 不存在 Layout 文件夹中");
            } finally {
                Bitmap bitmap = null;
                if (null != drawable) {
                    if (drawable instanceof BitmapDrawable) {
                        bitmap = BitmapFactory.decodeResource(context.getResources(), resID);
                    } else if (drawable instanceof VectorDrawable || drawable instanceof VectorDrawableCompat) {
                        int requireWidth = drawable.getIntrinsicWidth() <= 0 ? 1 : drawable.getIntrinsicWidth();
                        int requireHeight = drawable.getIntrinsicHeight() <= 0 ? 1 : drawable.getIntrinsicHeight();
                        bitmap = createBitmapSafely(requireWidth, requireHeight, Bitmap.Config.ARGB_8888, CREATE_BITMAP_RETRY_COUNT);
                        DEF_CANVAS.get().setBitmap(bitmap);
                        DEF_CANVAS.get().save();
                        DEF_CANVAS.get().drawColor(Color.TRANSPARENT);
                        drawable.setBounds(0, 0, requireWidth, requireHeight);
                        drawable.draw(DEF_CANVAS.get());
                        DEF_CANVAS.get().restore();
                        DEF_CANVAS.get().setBitmap(null);
                    }
                }
                if (null != rootView) {
                    if (processor != null) {
                        if (isFocus) {
                            processor.onFocusProcess(item, rootView, data);
                            processor.onFocusProcess(rootView, data);
                        } else {
                            processor.onNormalProcess(item, rootView, data);
                            processor.onNormalProcess(rootView, data);
                        }
                    }
                    rootView.measure(0, 0);
                    rootView.layout(0, 0, rootView.getMeasuredWidth(), rootView.getMeasuredHeight());
                    int requireWidth = (rootView.getMeasuredWidth() <= 0 ? 1 : rootView.getMeasuredWidth());
                    int requireHeight = (rootView.getMeasuredHeight() <= 0 ? 1 : rootView.getMeasuredHeight());
                    bitmap = createBitmapSafely(requireWidth, requireHeight, Bitmap.Config.ARGB_8888, CREATE_BITMAP_RETRY_COUNT);
                    DEF_CANVAS.get().setBitmap(bitmap);
                    DEF_CANVAS.get().save();
                    // 防止 View 上面有些区域空白导致最终 Bitmap 上有些区域变黑
                    DEF_CANVAS.get().drawColor(Color.TRANSPARENT);
                    rootView.draw(DEF_CANVAS.get());
                    DEF_CANVAS.get().restore();
                    DEF_CANVAS.get().setBitmap(null);
                }
                textureInfo = new TextureInfo(bitmap, resID);
            }
        }
        return textureInfo;
    }

    /**
     * 安全的创建bitmap;如果新建 Bitmap 时产生了 OOM，可以主动进行一次 GC - System.gc()，然后再次尝试创建
     *
     * @return bitmap
     */
    private Bitmap createBitmapSafely(int width, int height, Bitmap.Config config, int retryCount) {
        try {
            return Bitmap.createBitmap(width, height, config);
        } catch (Exception exception) {
            if (retryCount > 0) {
                System.gc();
                Logger.e(TAG, "创建布局纹理产生 OOM");
                return createBitmapSafely(width, height, config, retryCount - 1);
            }
            return null;
        }
    }
}

