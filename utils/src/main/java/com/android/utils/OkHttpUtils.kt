package com.android.utils

import android.annotation.SuppressLint
import android.content.Context
import android.content.res.Resources

import android.os.Handler
import android.os.Looper

import com.android.utils.log.Logger
import okhttp3.*
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.util.concurrent.TimeUnit

class OkHttpUtils private constructor() {
    private var mContext: Context? = null
    private var mResources: Resources? = null
    private val okHttpClient: OkHttpClient = OkHttpClient.Builder()
        .connectTimeout(10, TimeUnit.SECONDS)
        .readTimeout(10, TimeUnit.SECONDS)
        .writeTimeout(10, TimeUnit.SECONDS)
        .build()
        
    private lateinit var handler: Handler

    fun init(context: Context?) {
        mContext = context
        mResources = mContext!!.resources
        handler = Handler(Looper.getMainLooper())
    }

    /**
     * okHttp get异步请求
     */
    fun <T> getAsyncHttp(url: String, callback: OkHttpCallback<T>): Call? {
        return try {
            val request = Request.Builder()
                .url(url)
                .build()
            
            okHttpClient.newCall(request).apply {
                enqueue(object : Callback {
                    override fun onFailure(call: Call, e: IOException) {
                        failedCallBack("访问失败", callback)
                    }

                    override fun onResponse(call: Call, response: Response) {
                        if (response.isSuccessful) {
                            response.body?.string()?.let { result ->
                                Logger.i("返回结果：$result")
                                successCallBack(result as T, callback)
                            }
                        } else {
                            failedCallBack("服务器错误", callback)
                        }
                    }
                })
            }
        } catch (e: Exception) {
            Logger.e("请求异常: ${e.message}")
            null
        }
    }

    /**
     * post请求
     */
    fun <T> postAsyncHttp(url: String, callback: OkHttpCallback<T>, requestBody: RequestBody): Call? {
        return try {
            val request = Request.Builder()
                .url(url)
                .post(requestBody)
                .build()
                
            okHttpClient.newCall(request).apply {
                enqueue(object : Callback {
                    override fun onFailure(call: Call, e: IOException) {
                        failedCallBack("访问失败", callback)
                    }

                    override fun onResponse(call: Call, response: Response) {
                        if (response.isSuccessful) {
                            response.body?.string()?.let { result ->
                                Logger.i("返回结果：$result")
                                successCallBack(result as T, callback)
                            }
                        } else {
                            failedCallBack("服务器错误", callback)
                        }
                    }
                })
            }
        } catch (e: Exception) {
            Logger.e("请求异常: ${e.message}")
            null
        }
    }

    /**
     * 异步post表单提交
     */
    fun <T> postFromBody(map: Map<String, String>, url: String, callback: OkHttpCallback<T>): Call? {
        val builder = FormBody.Builder()
        map.forEach { (key, value) -> 
            builder.add(key, value)
        }
        return postAsyncHttp(url, callback, builder.build())
    }

    /**
     * 异步json格式提交数据
     */
    fun <T> postJson(map: Map<String, String>, url: String, callback: OkHttpCallback<T>): Call? {
        val jsonBody = map.entries.joinToString("&") { (key, value) -> 
            "$key=$value"
        }
        val mediaType = "application/json; charset=utf-8".toMediaTypeOrNull()
        val requestBody = RequestBody.create(mediaType, jsonBody)
        return postAsyncHttp(url, callback, requestBody)
    }

    /**
     * post上传单张图片
     */
    fun <T> postImage(file: File, url: String, callback: OkHttpCallback<T>): Call? {
        val requestBody = MultipartBody.Builder()
            .setType(MultipartBody.FORM)
            .addFormDataPart(
                "file",
                file.name,
                RequestBody.create("image/png".toMediaTypeOrNull(), file)
            )
            .build()
        return postAsyncHttp(url, callback, requestBody)
    }

    /**
     * 下载文件
     */
    fun <T> download(url: String, saveDir: String, callback: OkHttpCallback<T>) {
        val request = Request.Builder().url(url).build()
        okHttpClient.newCall(request).enqueue(object : Callback {
            override fun onFailure(call: Call, e: IOException) {
                failedCallBack("下载失败: ${e.message}", callback)
            }

            override fun onResponse(call: Call, response: Response) {
                var inputStream: InputStream? = null
                var outputStream: FileOutputStream? = null
                
                try {
                    val savePath = isExistDir(saveDir)
                    val file = File(savePath, getNameFromUrl(url))
                    
                    response.body?.let { body ->
                        inputStream = body.byteStream()
                        outputStream = FileOutputStream(file)
                        
                        val total = body.contentLength()
                        var sum = 0L
                        val buffer = ByteArray(1024)
                        var len: Int
                        
                        while (inputStream?.read(buffer).also { len = it ?: -1 } != -1) {
                            if (Thread.currentThread().isInterrupted) {
                                throw IOException("Download canceled")
                            }
                            
                            len.let {
                                outputStream?.write(buffer, 0, it)
                                sum += it
                                
                                if (total > 0) {
                                    val progress = (sum * 100 / total).toInt()
                                    onProgress(progress, callback)
                                }
                            }
                        }
                        
                        outputStream?.flush()
                        successCallBack("下载完成" as T, callback)
                    }
                } catch (e: Exception) {
                    Logger.e("下载失败: ${e.message}")
                    failedCallBack("下载失败", callback)
                } finally {
                    try {
                        inputStream?.close()
                        outputStream?.close()
                    } catch (e: IOException) {
                        Logger.e("关闭流失败")
                    }
                }
            }
        })
    }

    /**
     * 上传文件
     */
    fun <T> upLoadFile(url: String, filePath: String, callback: OkHttpCallback<T>) {
        val fileType = "File/*".toMediaTypeOrNull()
        val file = File(filePath)
        val body = RequestBody.create(fileType, file)
        val request = Request.Builder()
            .url(url)
            .post(body)
            .build()
            
        okHttpClient.newCall(request).enqueue(object : Callback {
            override fun onFailure(call: Call, e: IOException) {
                failedCallBack("filePath上传失败", callback)
            }

            override fun onResponse(call: Call, response: Response) {
                if (response.isSuccessful) {
                    response.body?.string()?.let { result ->
                        successCallBack(result as T, callback)
                    }
                } else {
                    failedCallBack("filePath上传失败", callback)
                }
            }
        })
    }

    private fun <T> successCallBack(result: T, callback: OkHttpCallback<T>) {
        handler.post { callback.onSuccess(result) }
    }

    private fun <T> failedCallBack(errorMsg: String, callback: OkHttpCallback<T>) {
        handler.post { callback.onFail(errorMsg) }
    }

    private fun <T> onProgress(progress: Int, callback: OkHttpCallback<T>) {
        handler.post { callback.onProgress(progress) }
    }

    private fun isExistDir(saveDir: String): String {
        val downloadFile = File(saveDir)
        if (!downloadFile.exists()) {
            downloadFile.mkdirs()
        }
        return downloadFile.absolutePath
    }

    private fun getNameFromUrl(url: String?): String {
        return url?.let {
            it.substring(it.lastIndexOf("/") + 1).takeIf { name ->
                name.isNotEmpty()
            }
        } ?: System.currentTimeMillis().toString()
    }

    interface OkHttpCallback<T> {
        fun onSuccess(result: T)
        fun onFail(error: String)
        fun onProgress(progress: Int)
    }

    companion object {
        @SuppressLint("StaticFieldLeak")
        @Volatile
        private var instance: OkHttpUtils? = null

        fun getInstance() = instance ?: synchronized(this) {
            instance ?: OkHttpUtils().also { instance = it }
        }
    }
}