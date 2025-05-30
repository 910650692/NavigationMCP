package com.fy.navi.hmi.utils;

import android.graphics.Bitmap;
import android.graphics.Matrix;
import android.graphics.Rect;

import androidx.annotation.MainThread;
import androidx.annotation.Nullable;
import androidx.annotation.WorkerThread;

import com.android.utils.ConvertUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AppContext;

import java.nio.ByteBuffer;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/5/29
 * Description: [截屏帮助类]
 */
public class CaptureScreenUtils {
    private static final String TAG = "CaptureScreenUtils";
    private CopyOnWriteArrayList<CaptureScreenCallBack> mListeners = new CopyOnWriteArrayList<>();

    private CaptureScreenUtils() {

    }

    private static class InstanceHolder {
        private final static CaptureScreenUtils instance = new CaptureScreenUtils();
    }

    public static CaptureScreenUtils getInstance() {
        return InstanceHolder.instance;
    }

    public interface CaptureScreenCallBack {
        /***
         * 图片裁剪成功回调
         * @param bitmap
         */
        @MainThread
        void onImageProcessCompleted(@Nullable Bitmap bitmap);
    }

    public void registerListener(final CaptureScreenCallBack callBack) {
        if (!ConvertUtils.isNull(callBack) && !mListeners.contains(callBack)) {
            mListeners.add(callBack);
            Logger.i(TAG, "registerListener success!");
        } else {
            Logger.e(TAG, "registerListener failed, callBack is null !");
        }
    }

    public void unRegisterListener(final CaptureScreenCallBack callBack) {
        if (!ConvertUtils.isNull(callBack) && mListeners.contains(callBack)) {
            Logger.i(TAG, "unRegisterListener success!");
            mListeners.remove(callBack);
        }
    }

    /***
     * 裁剪图片, 需要在子线程执行
     * @param bytes
     * @param rect
     * @return 返回一个期望的Bitmap
     */
    @WorkerThread
    public void processPicture(byte[] bytes, Rect rect) {
        ThreadManager.getInstance().execute(() -> {
            Bitmap orginBitmap = null;
            Bitmap flippedBitmap = null;
            Bitmap cropBitmap = null;
            try {
                // 注意：这个宽高要和API里面设置的裁剪区域大小一致
                int width = ScreenUtils.Companion.getInstance().getRealScreenWidth(AppContext.getInstance().getMContext());
                int height = ScreenUtils.Companion.getInstance().getRealScreenHeight(AppContext.getInstance().getMContext());
                orginBitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
                orginBitmap.copyPixelsFromBuffer(ByteBuffer.wrap(bytes));
                // 翻转图像
                Matrix matrix = new Matrix();
                matrix.postScale(1, -1);
                matrix.postTranslate(orginBitmap.getWidth(), orginBitmap.getHeight());
                flippedBitmap = Bitmap.createBitmap(orginBitmap, 0, 0, orginBitmap.getWidth(), orginBitmap.getHeight(), matrix, true);
                cropBitmap = Bitmap.createBitmap(flippedBitmap, rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top);// 数值来自UI（1.12-11）
            } catch (Exception e) {
                Logger.e(TAG, "processPicture failed:" + e.getMessage());
            } finally {
                if (!ConvertUtils.isNull(orginBitmap)) {
                    orginBitmap.recycle();
                }
                if (!ConvertUtils.isNull(flippedBitmap)) {
                    flippedBitmap.recycle();
                }
                Logger.d(TAG, "processPicture end!");
                notifyProcessCompleted(cropBitmap);
            }
        });
    }

    private void notifyProcessCompleted(final Bitmap bitmap) {
        ThreadManager.getInstance().postUi(() -> {
            mListeners.forEach(callBack -> {
                callBack.onImageProcessCompleted(bitmap);
            });
        });
    }
}
