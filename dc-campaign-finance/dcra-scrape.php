<?php

  include_once("simple_html_dom.php");

  function dcra($username, $password, $search) {
  $tmp_fname = tempnam('/tmp', 'COOKIE');

  $curl_handle = curl_init ('https://corp.dcra.dc.gov/Account.aspx/LogOn');

  curl_setopt($curl_handle, CURLOPT_COOKIEJAR, $tmp_fname);
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

  $login_fields = array(
    'username'=>$username,
    'password'=>$password,
    'LogOn'=>'Log On'
  );

  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $login_fields);

  $login_output = curl_exec($curl_handle);
//  file_put_contents('login.html', $login_output);

  $curl_handle = curl_init ('https://corp.dcra.dc.gov/Home.aspx/ProcessRequest');
  curl_setopt($curl_handle, CURLOPT_COOKIEFILE, $tmp_fname);
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

  $fields = array(
    'BizEntitySearch_String'=>$search, 
    'Search'=>'Search',
    'BizEntitySearch_Type'=>'EntityName',
    'BizEntitySearch_DepthType'=>'Contains',
    'BizEntitySearch_EntityStatus'=>'',
    'BizEntitySearch_TradeNameStatus'=>''
  );

  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $fields);

  $query_output = curl_exec($curl_handle);
//  file_put_contents('dcra.html', $query_output);

  preg_match_all("|/BizEntity.aspx(.*)\w+(?=\")|U",
    $query_output,
    $out, PREG_PATTERN_ORDER);

  foreach ($out[0] as $leafurl) {

    $fullurl = "https://corp.dcra.dc.gov" . $leafurl;

    $curl_handle = curl_init ($fullurl);
    curl_setopt($curl_handle, CURLOPT_COOKIEFILE, $tmp_fname);
    curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);

    $leaf_output = curl_exec($curl_handle);
//    file_put_contents('dcra_corp.html', $leaf_output);
    
    $leaf_output = str_get_html($leaf_output)->plaintext;
    
//  echo file_get_html('/Users/travismcarthur/git/Occupy-data-processing/dc-campaign-finance/dcra_corp.html')->plaintext;
//    $html = file_get_html('/Users/travismcarthur/git/Occupy-data-processing/dc-campaign-finance/dcra_corp.html')->plaintext;
  
    $html = preg_replace("/\t/" , " ", $leaf_output);
	$html = preg_replace("/\r/" , " ", $html);
	$html = preg_replace("/\n/" , " ", $html);
    $html = preg_replace("/(         )+/" , "<SEPARATOR>", $html);
    $html = preg_replace("/<SEPARATOR>(\s)+/" , "<SEPARATOR>", $html);
    $html = preg_replace("/(<SEPARATOR>)+/" , "<SEPARATOR>", $html);

    $fp = fopen('dcra_temp_data.txt', 'a');
    fwrite($fp, $html . "\r\n");

    echo $fullurl . "\n";
  }
}

	
//	echo 'Enter username:';
	$stdin = fopen('user.txt', 'r');
	$username = trim(fgets($stdin));
	
//	echo 'Enter password:';
	$stdin = fopen('pw.txt', 'r');
	$password = trim(fgets($stdin));
	
//	echo 'Enter search string:';
	$stdin = fopen('dcraQuery.txt', 'r');
	$search = trim(fgets($stdin));
	
	if ($username && $password && $search) {
		dcra($username, $password, $search);
	} else {
		echo 'Invalid username, password or search query.';
	}

?>