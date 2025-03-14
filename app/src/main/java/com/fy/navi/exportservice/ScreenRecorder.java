package com.fy.navi.exportservice;

import android.annotation.SuppressLint;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
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
import android.util.Log;

import androidx.annotation.Nullable;

import com.fy.navi.fsa.R;
import com.fy.navi.hud.VTBinder;
import com.iauto.vtserver.VTDescription;
import com.iauto.vtserver.VTServerBQJni;

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

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
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
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent == null) {
            return super.onStartCommand(intent, flags, startId);
        }
        createNotificationChannel();
        int mResultCode = intent.getIntExtra("code", -1);
        Intent mResultData = intent.getParcelableExtra("data");
        //mResultData = intent.getSelector();
        mMediaProjectionManager = (MediaProjectionManager) getSystemService(Context.MEDIA_PROJECTION_SERVICE);
        mMediaProjection = mMediaProjectionManager.getMediaProjection(mResultCode, Objects.requireNonNull(mResultData));

        Log.i(TAG, "mMediaProjection created: " + mMediaProjection);

        DisplayMetrics metrics = getResources().getDisplayMetrics();
        mDensity = metrics.densityDpi;

        //可以通过修改mWidth和mHeight控制屏幕大小，上面注释的代码是获取屏幕默认大小
        //由于virtualdisplay限制，mWidth只能取部分值，mHeight需要时4的倍数，泛亚提供的屏幕代销
        //所以如果想自己修改分辨率，需要打印ImageAvailableListener里的length计算出实际分辨率
        //每个像素4字节，length/4/mHeight就是mWidth。
        //泛亚提供的大屏，分辨率识别出的是1790*1080，宽不是4的倍数，建议写死为1788*1080

        // start capture reader
        //PixelFormat.RGBA_8888格式可以指定为其他的
        mImageReader = ImageReader.newInstance(metrics.widthPixels, metrics.heightPixels, PixelFormat.RGBA_8888, 2);
//        mVirtualDisplay = mMediaProjection.createVirtualDisplay(SCREENCAP_NAME, mWidth, mHeight, mDensity, DisplayManager.VIRTUAL_DISPLAY_FLAG_AUTO_MIRROR, mImageReader.getSurface(), null, null);
        mMediaProjection.registerCallback(mpCallback, null);
        mVirtualDisplay = mMediaProjection.createVirtualDisplay(SCREENCAP_NAME, metrics.widthPixels, metrics.heightPixels, mDensity, DisplayManager.VIRTUAL_DISPLAY_FLAG_AUTO_MIRROR, mImageReader.getSurface(), null, null);
        mImageReader.setOnImageAvailableListener(new ImageAvailableListener(), null);

        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public void init() {
        Log.d(TAG, "service init");

        // 1. NativeInitialize
        int ret = VTServerBQJni.getInstance().nativeInitialize();
        Log.d(TAG, "NativeInitialize ret is " + ret);

        // 2. NativeSetVideoDescription
        VTDescription description = new VTDescription();
        description.width = mWidth;
        description.height = mHeight;
        description.videoFormat = 0x901001;  // PixelFormat.RGBA_8888;
        VTServerBQJni.getInstance().nativeSetVideoDescription(description);
        Log.d(TAG, "NativeSetVideoDescription");
    }

    public void start() {
        Log.d(TAG, "service start");
        if (!mStartFlag) {
            mStartFlag = true;

            // 3. NativeStart
            int ret = VTServerBQJni.getInstance().nativeStart();
            Log.d(TAG, "service start NativeStart ret is " + ret);
        }
    }

    public void stop() {
        Log.d(TAG, "service stop");
        if (mStartFlag) {
            mStartFlag = false;

            // 4. NativeStop
            int ret = VTServerBQJni.getInstance().nativeStop();
            Log.d(TAG, "service stop NativeStop ret is " + ret);
        }
    }

    public void uninit() {
        Log.d(TAG, "service uninit");

        // 6. NativeUninitialize
        VTServerBQJni.getInstance().nativeUninitialize();
    }

    int codeNum = 1;
    public void notifyError() {
        // 7. NativeNotifyError
        if (codeNum > 10) {
            codeNum = 1;
        } else {
            ++codeNum;
        }
        VTServerBQJni.getInstance().nativeNotifyError(codeNum, "test error code");
        Log.d(TAG, "service notify error[" + codeNum + "]");
    }

    // Binder


    // Depend on change!!!
    private class ImageAvailableListener implements ImageReader.OnImageAvailableListener {
        // 控制帧率用，实际测试效果不佳，
        // 视频通话控制在15帧是足够的
        @Override
        public void onImageAvailable(ImageReader reader) {
            try (Image image = reader.acquireLatestImage()) {
                if (image != null) {
                    Thread.sleep(15);  // 15 ms

                    int width = image.getWidth();
                    int height = image.getHeight();
                    // 实测最大帧率为63FPS，可以处理丢弃部分帧达到控制帧率的目的
                    // 获取image bytes
                    Image.Plane[] planes = image.getPlanes();
                    ByteBuffer buffer = planes[0].getBuffer();

                    int pixelStride = planes[0].getPixelStride();
                    int rowStride = planes[0].getRowStride();

                    // 定义裁剪区域
                    int startX = (width - mWidth) / 2 + 190; // 起始X坐标
                    int startY = (height - mHeight) / 2 + 50; // 起始Y坐标

                    // 创建一个新的字节数组来存储裁剪后的图像数据
                    byte[] croppedData = new byte[mWidth * mHeight * pixelStride];

                    // 提取裁剪区域的有效像素数据
                    for (int i = 0; i < mHeight; i++) {
                        int bufferPosition = (startY + i) * rowStride + startX * pixelStride;
                        int arrayPosition = i * mWidth * pixelStride;
                        buffer.position(bufferPosition);
                        buffer.get(croppedData, arrayPosition, mWidth * pixelStride);
                    }
                    // 5. NativeNotifyVideoData
                    if (mStartFlag) {
                        VTServerBQJni.getInstance().nativeNotifyVideoData(croppedData);
                    }
                    // 处理完一张图片需要close
                    image.close();
                    Log.d(TAG,  "onImageAvailable Done length is " + croppedData.length);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private void createNotificationChannel() {
        Notification.Builder builder = new Notification.Builder(this.getApplicationContext()); //获取一个Notification构造器
//        Intent nfIntent = new Intent(this, StartupActivity.class); //点击后跳转的界面，可以设置跳转数据
        Intent nfIntent = new Intent(); //点击后跳转的界面，可以设置跳转数据

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
            NotificationManager notificationManager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
            NotificationChannel channel = new NotificationChannel("notification_id", "notification_name", NotificationManager.IMPORTANCE_LOW);
            notificationManager.createNotificationChannel(channel);
        }

        Notification notification = builder.build(); // 获取构建好的Notification
        notification.defaults = Notification.DEFAULT_SOUND; //设置为默认的声音
        startForeground(110, notification);
    }
}
