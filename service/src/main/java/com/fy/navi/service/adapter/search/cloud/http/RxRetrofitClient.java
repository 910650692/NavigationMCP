package com.fy.navi.service.adapter.search.cloud.http;



import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.concurrent.TimeUnit;

import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import retrofit2.Retrofit;
import retrofit2.adapter.rxjava.RxJavaCallAdapterFactory;
import retrofit2.converter.gson.GsonConverterFactory;

/**
 * @version \$Revision1.0\$
 * @author baipeng0904
 * <pre>
 *     author : BaiPengMac
 *     e-mail : xxx@xx
 *     time   : 2022/04/13
 *     desc   : OkHttp简单封装
 *     version: xx
 * </pre>
 */
public final class RxRetrofitClient {
    private static volatile RxRetrofitClient mInstance;
    private Retrofit mRetrofit;
    private static final long DEFAULT_CONNECT_TIMEOUT = 15 * 1000;

    /**
     * Init client.
     *
     * @return RxRetrofitClient
     */
    public static RxRetrofitClient getInstance() {
        if (null == mInstance) {
            synchronized (RxRetrofitClient.class) {
                if (null == mInstance) {
                    mInstance = new RxRetrofitClient();
                }
            }
        }
        return mInstance;
    }

    /**
     * 初始化Retrofit对象
     */
    private void init() {
        this.mRetrofit = newRetrofit();
    }

    private RxRetrofitClient() {
        init();
    }

    /**
     * 创建retrofit service代理
     * @param service service类型的Class对象
     * @return retrofit service代理
     * @param <T> 泛型参数
     */
    public <T> T create(final Class<T> service) {
        if (mInstance == null) {
            throw new NullPointerException("未初始化RxRetrofitClient");
        }
        return mRetrofit.create(service);
    }

    /**
     * 创建retrofit对象
     * @return retrofit对象
     */
    private Retrofit newRetrofit() {
        final Retrofit.Builder builder = new Retrofit.Builder();
        builder.baseUrl(HttpConstants.AppKeyProd.BASE_CLOUD_URL);
        builder.addConverterFactory(GsonConverterFactory.create());
        builder.addCallAdapterFactory(RxJavaCallAdapterFactory.create());
        builder.client(getOkHttpClient()).build();
        return builder.build();
    }

    /**
     * 创建OkHttpClient对象
     * @return OkHttpClient对象
     */
    private OkHttpClient getOkHttpClient() {
        final HttpLoggingInterceptor loggingInterceptor = new HttpLoggingInterceptor(
                message -> Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG + "==OKHttp: ", "Message: " + message));
        loggingInterceptor.setLevel(HttpLoggingInterceptor.Level.BODY);

        final OkHttpClient.Builder builder = new OkHttpClient.Builder();
        builder.connectTimeout(DEFAULT_CONNECT_TIMEOUT, TimeUnit.MILLISECONDS);
        builder.addInterceptor(loggingInterceptor);
        builder.addInterceptor(new SignInterceptor());
        return builder.build();
    }
}
