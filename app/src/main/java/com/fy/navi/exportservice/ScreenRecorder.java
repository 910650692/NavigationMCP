package com.fy.navi.exportservice;

import android.annotation.SuppressLint;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.PixelFormat;
import android.hardware.display.DisplayManager;
import android.hardware.display.VirtualDisplay;
import android.media.Image;
import android.media.ImageReader;
import android.media.projection.MediaProjection;
import android.media.projection.MediaProjectionManager;
import android.os.Build;
import android.os.IBinder;
import android.util.DisplayMetrics;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.fsa.R;
import com.fy.navi.hud.VTBinder;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.iauto.vtserver.VTDescription;
import com.iauto.vtserver.VTServerBQJni;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.util.Objects;

public class ScreenRecorder extends Service {

    private MediaProjectionManager mMediaProjectionManager;
    private MediaProjection mMediaProjection;
    private ImageReader mImageReader;
    private VirtualDisplay mVirtualDisplay;

    private static final String SCREENCAP_NAME = "screencap";
    private static final String TAG = ScreenRecorder.class.getSimpleName();
    private int mDensity;
    private int mWidth = 328;
    private int mHeight = 172;
    private boolean mStartFlag = false;
    private byte[] mCrossImg;

    @Nullable
    @Override
    public IBinder onBind(final Intent intent) {
        return new VTBinder() {
            @Override
            public void init() {
                ScreenRecorder.this.init();
            }

            @Override
            public void start() {
                ScreenRecorder.this.start();
            }

            @Override
            public void stop() {
                ScreenRecorder.this.stop();
            }

            @Override
            public void uninit() {
                ScreenRecorder.this.uninit();
            }


            @Override
            public void notifyError() {
                ScreenRecorder.this.notifyError();
            }

            @Override
            public byte[] getCrossImg() {
                return getmCrossImg();
            }
        };
    }

    private MediaProjection.Callback mpCallback = new MediaProjection.Callback() {
        @Override
        public void onStop() {
            super.onStop();
        }
    };

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @SuppressLint("WrongConstant")
    @Override
    public int onStartCommand(final Intent intent, final int flags, final int startId) {
        if (intent == null) {
            return super.onStartCommand(intent, flags, startId);
        }
        createNotificationChannel();
        final int resultCode = intent.getIntExtra("code", -1);
        final Intent resultData = intent.getParcelableExtra("data");
        //resultData = intent.getSelector();
        mMediaProjectionManager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
        mMediaProjection = mMediaProjectionManager.getMediaProjection(resultCode, Objects.requireNonNull(resultData));

        Logger.i(TAG, "mMediaProjection created: " + mMediaProjection);

        final DisplayMetrics metrics = getResources().getDisplayMetrics();
        mDensity = metrics.densityDpi;

        //可以通过修改mWidth和mHeight控制屏幕大小，上面注释的代码是获取屏幕默认大小
        //由于virtualdisplay限制，mWidth只能取部分值，mHeight需要时4的倍数，泛亚提供的屏幕代销
        //所以如果想自己修改分辨率，需要打印ImageAvailableListener里的length计算出实际分辨率
        //每个像素4字节，length/4/mHeight就是mWidth。
        //泛亚提供的大屏，分辨率识别出的是1790*1080，宽不是4的倍数，建议写死为1788*1080

        // start capture reader
        //PixelFormat.RGBA_8888格式可以指定为其他的
        mImageReader = ImageReader.newInstance(metrics.widthPixels, metrics.heightPixels, PixelFormat.RGBA_8888, 2);
//        mVirtualDisplay = mMediaProjection.createVirtualDisplay(SCREENCAP_NAME, mWidth, mHeight, mDensity,
//        DisplayManager.VIRTUAL_DISPLAY_FLAG_AUTO_MIRROR, mImageReader.getSurface(), null, null);
        mMediaProjection.registerCallback(mpCallback, null);
        mVirtualDisplay = mMediaProjection.createVirtualDisplay(SCREENCAP_NAME, metrics.widthPixels,
                metrics.heightPixels, mDensity, DisplayManager.VIRTUAL_DISPLAY_FLAG_AUTO_MIRROR, mImageReader.getSurface(), null, null);
        mImageReader.setOnImageAvailableListener(new ImageAvailableListener(), null);

        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * 初始化
     */
    public void init() {
        Logger.d(TAG, "service init");
        if (!VTServerBQJni.getInstance().isIsSuccessLoadLibrary()) {
            Logger.d(TAG, "the so library failed to load");
            return;
        }

        // 1. NativeInitialize
        final int ret = VTServerBQJni.getInstance().nativeInitialize();
        Logger.d(TAG, "NativeInitialize ret is " + ret);

        // 2. NativeSetVideoDescription
        final VTDescription description = new VTDescription();
        description.width = mWidth;
        description.height = mHeight;
        description.videoFormat = 0x901001;  // PixelFormat.RGBA_8888;
        VTServerBQJni.getInstance().nativeSetVideoDescription(description);
        Logger.d(TAG, "NativeSetVideoDescription");
    }

    /**
     * 开始
     */
    public void start() {
        Logger.d(TAG, "service start");
        if (!mStartFlag) {
            mStartFlag = true;

            // 3. NativeStart
            final int ret = VTServerBQJni.getInstance().nativeStart();
            Logger.d(TAG, "service start NativeStart ret is " + ret);
        }
    }

    /**
     * 停止
     */
    public void stop() {
        Logger.d(TAG, "service stop");
        if (mStartFlag) {
            mStartFlag = false;

            // 4. NativeStop
            final int ret = VTServerBQJni.getInstance().nativeStop();
            Logger.d(TAG, "service stop NativeStop ret is " + ret);
        }
    }

    /**
     * 销毁
     */
    public void uninit() {
        Logger.d(TAG, "service uninit");

        // 6. NativeUninitialize
        VTServerBQJni.getInstance().nativeUninitialize();
    }

    private int mCodeNum = 1;

    /**
     * 通知错误
     */
    public void notifyError() {
        // 7. NativeNotifyError
        if (mCodeNum > 10) {
            mCodeNum = 1;
        } else {
            ++mCodeNum;
        }
        VTServerBQJni.getInstance().nativeNotifyError(mCodeNum, "test error code");
        Logger.d(TAG, "service notify error[" + mCodeNum + "]");
    }

    // Binder


    // Depend on change!!!
    private final class ImageAvailableListener implements ImageReader.OnImageAvailableListener {
        // 控制帧率用，实际测试效果不佳，
        // 视频通话控制在15帧是足够的
        @Override
        public void onImageAvailable(final ImageReader reader) {
            try (Image image = reader.acquireLatestImage()) {
                if (image != null) {
                    Thread.sleep(15);  // 15 ms
                    final byte[] bytes = imageCropping(image);
                    saveRoadLarge(image);
                    // 5. NativeNotifyVideoData
                    if (mStartFlag) {
                        VTServerBQJni.getInstance().nativeNotifyVideoData(bytes);
                    }
                    // 处理完一张图片需要close
                    image.close();
                }
            } catch (InterruptedException | IllegalStateException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * 路网图裁剪
     * @param image
     * @return 数据
     */
    private byte[] imageCropping(final Image image) {
        if (!NaviStatus.NaviStatusType.NAVING.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            return new byte[225664];
        }
        final int width = image.getWidth();
        final int height = image.getHeight();
        // 实测最大帧率为63FPS，可以处理丢弃部分帧达到控制帧率的目的
        // 获取image bytes
        final Image.Plane[] planes = image.getPlanes();
        final ByteBuffer buffer = planes[0].getBuffer();

        final int pixelStride = planes[0].getPixelStride();
        final int rowStride = planes[0].getRowStride();

        // 定义裁剪区域
        final int startX = (width - mWidth) / 2 + 190; // 起始X坐标
        final int startY = (height - mHeight) / 2 + 70; // 起始Y坐标

        // 创建一个新的字节数组来存储裁剪后的图像数据
        final byte[] croppedData = new byte[mWidth * mHeight * pixelStride];

        // 提取裁剪区域的有效像素数据
        for (int i = 0; i < mHeight; i++) {
            final int bufferPosition = (startY + i) * rowStride + startX * pixelStride;
            final int arrayPosition = i * mWidth * pixelStride;
            buffer.position(bufferPosition);
            buffer.get(croppedData, arrayPosition, mWidth * pixelStride);
        }
        return croppedData;
    }

    /**
     * 保存路口大图
     * @param image
     */
    private void saveRoadLarge(final Image image) {
        // 实测最大帧率为63FPS，可以处理丢弃部分帧达到控制帧率的目的
        // 获取image bytes
        final Image.Plane[] planes = image.getPlanes();
        final ByteBuffer buffer = planes[0].getBuffer();

        final int pixelStride = planes[0].getPixelStride();
        final int rowStride = planes[0].getRowStride();

        // 定义裁剪区域
        final int startX = 600; // 起始X坐标
        final int startY = 170; // 起始Y坐标
        // 创建一个新的字节数组来存储裁剪后的图像数据
        final byte[] data = new byte[500 * 300 * pixelStride];

        // 提取裁剪区域的有效像素数据
        for (int i = 0; i < 300; i++) {
            final int bufferPosition = (startY + i) * rowStride + startX * pixelStride;
            final int arrayPosition = i * 500 * pixelStride;
            buffer.position(bufferPosition);
            buffer.get(data, arrayPosition, 500 * pixelStride);
        }
        // 将 croppedData 转换为 Bitmap
        final Bitmap bitmap = Bitmap.createBitmap(500, 300, Bitmap.Config.ARGB_8888);
        bitmap.copyPixelsFromBuffer(ByteBuffer.wrap(data));
        // Compress Bitmap to JPG
        final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        bitmap.compress(Bitmap.CompressFormat.JPEG, 100, byteArrayOutputStream);
        mCrossImg = byteArrayOutputStream.toByteArray();
    }

    /**
     * 获取路口大图
     *
     * @return 路口大图
     */
    public byte[] getmCrossImg() {
        return mCrossImg;
    }

    /**
     * 创建通知栏
     */
    private void createNotificationChannel() {
        final Notification.Builder builder = new Notification.Builder(this.getApplicationContext()); //获取一个Notification构造器
//        Intent nfIntent = new Intent(this, StartupActivity.class); //点击后跳转的界面，可以设置跳转数据
        final Intent nfIntent = new Intent(); //点击后跳转的界面，可以设置跳转数据

        builder.setContentIntent(PendingIntent.getActivity(this, 0, nfIntent, PendingIntent.FLAG_IMMUTABLE)) // 设置PendingIntent
                .setLargeIcon(BitmapFactory.decodeResource(this.getResources(), R.mipmap.ic_launcher)) // 设置下拉列表中的图标(大图标)
                //.setContentTitle("SMI InstantView") // 设置下拉列表里的标题
                .setSmallIcon(R.mipmap.ic_launcher) // 设置状态栏内的小图标
                .setContentText("is running......") // 设置上下文内容
                .setWhen(System.currentTimeMillis()); // 设置该通知发生的时间

        /*以下是对Android 8.0的适配*/
        //普通notification适配
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            builder.setChannelId("notification_id");
        }
        //前台服务notification适配
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            final NotificationManager notificationManager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
            final NotificationChannel channel = new NotificationChannel("notification_id", "notification_name", NotificationManager.IMPORTANCE_LOW);
            notificationManager.createNotificationChannel(channel);
        }

        final Notification notification = builder.build(); // 获取构建好的Notification
        notification.defaults = Notification.DEFAULT_SOUND; //设置为默认的声音
        startForeground(110, notification);
    }
}
